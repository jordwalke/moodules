let pwd = Sys.getcwd() ++ "\\";
/* This should be done in Fp.fromSystemDir(~os=CurrentOS|Windows) */
let pwd =
  Str.global_replace(Str.regexp("\\\\\\([^\\\\]\\|$\\)?"), "/\\1", pwd);
let commentRe = Str.regexp("[ \t];.*$");
let publicNameRe = Str.regexp("([ \t]*public_name[ \t]*\\([^)]*\\)");
let implementsRe = Str.regexp("([ \t]*extends_moodule[ \t]*\\([^)]*\\)");
let mooduleAbstractRe = Str.regexp("([ \t]*abstract_moodule[ \t]*)");
let includeUnqualified =
  Str.regexp("([ \t]*include_subdirs[ \t]*\\unqualified[ \t]*)");
let dotRe = Str.regexp("\\.");

let duneDir = Fp.absoluteExn(pwd);
let baseName = Fp.baseName(duneDir);
let parentDir = Fp.dirName(duneDir);

let mooduleConfigPath = Fp.append(duneDir, "dune");
let fileContents = String.concat("\n", Fs.readTextExn(mooduleConfigPath));
let configCloseRegexp = Str.regexp("|}");
let configOpenRegexp = Str.regexp("{|");
let lastIndex = String.length(fileContents) - 1;
let range =
  switch (Str.search_backward(configCloseRegexp, fileContents, lastIndex)) {
  | exception Not_found => None
  | end_ =>
    switch (Str.search_backward(configOpenRegexp, fileContents, end_)) {
    | exception Not_found => None
    | start => Some((start + 2, end_ - 2))
    }
  };

/* switch (baseName) { */
/* | Some("virtual") => () */
/* | Some(_) */
/* | None => */
/*   switch (Fs.readDir(Fp.At.(parentDir / "virtual"))) { */
/*   | Error(e) => () */
/*   | Ok(fps) => fps */
/*   } */
/* }; */

let origDuneConfig =
  switch (range) {
  | None =>
    [
      "(library",
      "  (public_name \"virtualish.llfoobar\")",
      "  (name foo)",
      ")",
    ]
    |> String.concat("\n")
  | Some((start, end_)) => String.sub(fileContents, start, end_ - start + 1)
  };

/**
 * Remove any (include_subdirs unqualified) because we are going to have to
 * copy all deeper files into the root directory in order to join files from
 * multiple locations.
 */
let origDuneConfigNoComments =
  Str.global_replace(commentRe, "", origDuneConfig);

let hasUnqualified = s =>
  switch (Str.search_forward(includeUnqualified, s, 0)) {
  | exception Not_found => false
  | i => true
  };

let origDuneHasUnqualified = hasUnqualified(origDuneConfigNoComments);

let origDuneMinusUnqualified =
  Str.global_replace(includeUnqualified, "", origDuneConfigNoComments);

/* let parsedConfig = */
/*   Sexpression.s_expression_of_token_list( */
/*     Sexpression.tokenize("(" ++ origDuneMinusUnqualified ++ ")"), */
/*   ); */

let extractPublicName = s =>
  switch (Str.search_forward(publicNameRe, s, 0)) {
  | exception Not_found => None
  | i =>
    let _matched = Str.string_match(publicNameRe, s, i);
    Some(Str.matched_group(1, s));
  };

let extractUnqualified = s =>
  switch (Str.search_forward(includeUnqualified, s, 0)) {
  | exception Not_found => false
  | i => true
  };

let extractImplements = s =>
  switch (Str.search_forward(implementsRe, s, 0)) {
  | exception Not_found => None
  | i =>
    let _matched = Str.string_match(implementsRe, s, i);
    Some(Str.matched_group(1, s));
  };

/**
 * Abstract moodule libraries are not and cannot be used - except in order
 * to extend them. This means they can have `.rei` files without corresponding
 * `.re` files, or other incomplete features that are completed in extensions.
 */
let extractIsAbstractMoodule = s =>
  switch (Str.search_forward(mooduleAbstractRe, s, 0)) {
  | exception Not_found => false
  | i => true
  };

let publicName =
  switch (extractPublicName(origDuneMinusUnqualified)) {
  | None => "nopublicname"
  | Some(pn) => pn
  };

let implementsString =
  switch (extractImplements(origDuneMinusUnqualified)) {
  | None => "nopublicname"
  | Some(pn) => pn
  };

let origDuneMinusImplements =
  Str.replace_first(
    implementsRe,
    "\n ; implements " ++ implementsString ++ " handled by moodule",
    origDuneMinusUnqualified,
  );
let (implements, isAbstract) = (
  extractImplements(origDuneMinusUnqualified),
  extractIsAbstractMoodule(origDuneMinusUnqualified),
);

if (isAbstract) {
  print_endline("; Abstract moodule");
  exit(0);
};

let alreadyInTarget = Hashtbl.create(7);

let allRelSourcesAlreadyInTarget = {
  let onNode = (queryResult: Fs.queryResult, cont) => {
    switch (queryResult) {
    | File(path, stats) =>
      Hashtbl.add(
        alreadyInTarget,
        Fp.relativizeExn(~source=duneDir, ~dest=path),
        true,
      )
    | Dir(path, _) =>
      if (Fp.eq(path, duneDir) || origDuneHasUnqualified) {
        cont();
      }
    | Other(_) => ()
    | Link(path, toPath, stats) => ()
    };
  };
  Fs.traverseFileSystemFromPath(~onNode, duneDir);
};

let pathsToCopyFromInside =
  Hashtbl.fold(
    (key, _, cur) => {
      !Fp.eq(Fp.dirName(Fp.join(duneDir, key)), duneDir)
        ? [(duneDir, Fp.join(duneDir, key)), ...cur] : cur
    },
    alreadyInTarget,
    [],
  );

let pathsToCopyFromOutsideDir = (unqualified, fromDir) => {
  let toCopySet = Hashtbl.create(7);
  let onNode = (queryResult: Fs.queryResult, cont) => {
    switch (queryResult) {
    | File(path, stats) =>
      let rel = Fp.relativizeExn(~source=fromDir, ~dest=path);
      if (Hashtbl.find_opt(alreadyInTarget, rel) === None) {
        // Fp.baseName(rel) != Some("VirtualInterfaceInSubdir.rei") &&
        Hashtbl.add(
          toCopySet,
          (fromDir, path),
          true,
        );
      };
    | Dir(path, _) =>
      if (Fp.eq(path, fromDir) || unqualified) {
        cont();
      }
    | Other(_) => ()
    | Link(path, toPath, stats) => ()
    };
  };
  Fs.traverseFileSystemFromPath(~onNode, fromDir);
  toCopySet;
};

let siblingDirs = Fs.readDirExn(parentDir);
let absolutePathsToCopy =
  List.fold_left(
    (absolutePathsToCopy, sibling) =>
      if (Fp.eq(sibling, duneDir)) {
        absolutePathsToCopy;
      } else {
        switch (implements) {
        | None => absolutePathsToCopy
        | Some(im) =>
          switch (Fs.readText(Fp.append(sibling, "dune"))) {
          | Error(_) => absolutePathsToCopy
          | Ok(lines) =>
            let txt = String.concat("\n", lines);
            switch (extractPublicName(txt)) {
            | None => absolutePathsToCopy
            | Some(pn) =>
              if (String.equal(pn, im)) {
                let otherLibIsUnqualified = hasUnqualified(txt);
                let allAbsSourcesToCopyFromSibling =
                  pathsToCopyFromOutsideDir(otherLibIsUnqualified, sibling);
                let more =
                  Hashtbl.fold(
                    (key, _, cur) => {[key, ...cur]},
                    allAbsSourcesToCopyFromSibling,
                    [],
                  );
                more @ absolutePathsToCopy;
              } else {
                absolutePathsToCopy;
              }
            };
          }
        };
      },
    [],
    siblingDirs,
  );

let publicNameDebug = Str.global_replace(dotRe, "--", publicName);

let copyFileToTopLevelOfLibraryRule = ((otherLibRoot, absSource)) => {
  let relFromLib = Fp.relativizeExn(~source=duneDir, ~dest=absSource);
  let baseName =
    switch (Fp.baseName(absSource)) {
    | None => "CannotDetermineBaseNameOfFile_ReportThisError.txt"
    | Some(bn) => bn
    };
  "(rule "
  ++ "(target "
  ++ baseName
  ++ ")"
  ++ "(action (copy# "
  ++ Fp.toDebugString(relFromLib)
  ++ " "
  ++ baseName
  ++ ")))";
};
let copyLines =
  List.map(copyFileToTopLevelOfLibraryRule, absolutePathsToCopy);
let copyLinesInternal =
  List.map(copyFileToTopLevelOfLibraryRule, pathsToCopyFromInside);
let copyString =
  switch (implements, copyLines) {
  | (Some(im), []) =>
    if (String.equal(im, publicName)) {
      "\n(library-named---"
      ++ publicName
      ++ "---claims-to-implement-itself--That-does-not-make-sense)";
    } else {
      "\n(i-cannot-find-the-virtual-library---"
      ++ im
      ++ "---supposedly-implemented-by---"
      ++ publicName
      ++ "---make-sure-it-is-a-sibling)";
    }
  | (Some(_), [_, ..._])
  | (None, []) =>
    String.concat("\n", ["", ...copyLines @ copyLinesInternal])
  | (None, [_, ..._]) =>
    "\n(internal-error-library---"
    ++ publicName
    ++ "---should-not-be-copying-over-files-it-doesnt-implement-a-virtual-library)"
  };

let everythingButDebug = origDuneMinusImplements ++ copyString;
let debugLines = [
  "",
  "; DEBUG COMMAND",
  "(alias (name debug-"
  ++ publicNameDebug
  ++ ") (action (echo \""
  ++ everythingButDebug
  ++ "\")",
  "))",
];
let debugLines =
  String.split_on_char('\n', String.concat("\n", debugLines))
  |> List.map(s => "        " ++ s);

let debugString = String.concat("\n", debugLines);
/* This was changed in dune 2.0 */
let debugString = "";

print_endline(everythingButDebug);
print_endline(debugString ++ "\n");
exit(0);
