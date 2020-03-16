let pwd = Sys.getcwd() ++ "\\";
/* This should be done in Fp.fromSystemDir(~os=CurrentOS|Windows) */
let pwd =
  Str.global_replace(Str.regexp("\\\\\\([^\\\\]\\|$\\)?"), "/\\1", pwd);
let commentRe = Str.regexp("[ \t];.*$");
let publicNameRe = Str.regexp("([ \t]*public_name[ \t]*\\([^)]*\\)");
let extendsRe = Str.regexp("([ \t]*extends_moodule[ \t]*\\([^)]*\\))");
let includeUnqualifiedRe =
  Str.regexp("([ \t]*include_subdirs[ \t]*\\unqualified[ \t]*)");
let dirsRe = Str.regexp("([ \t]*dirs[ \t]*\\([^)]*\\)");
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

let stripComments = s => Str.global_replace(commentRe, "", s);

let stripExtends = (s, extends) =>
  Str.replace_first(
    extendsRe,
    "\n ; extends " ++ extends ++ " handled by moodule",
    s,
  );

/* switch (baseName) { */
/* | Some("virtual") => () */
/* | Some(_) */
/* | None => */
/*   switch (Fs.readDir(Fp.At.(parentDir / "virtual"))) { */
/*   | Error(e) => () */
/*   | Ok(fps) => fps */
/*   } */
/* }; */

let duneConfig =
  switch (range) {
  | None =>
    [
      "(error \"The dune file in "
      ++ Fp.toDebugString(duneDir)
      ++ " uses the Moodules plugin but doesn't include any manual dune config. "
      ++ "Include a {|..|} string with your actual dune config somewhere in the file. Doesn't matter where\")",
    ]
    |> String.concat("\n")
  | Some((start, end_)) => String.sub(fileContents, start, end_ - start + 1)
  };

/**
 * Remove any (include_subdirs unqualified) because we are going to have to
 * copy all deeper files into the root directory in order to join files from
 * multiple locations.
 */
let duneConfig = stripComments(duneConfig);

let hasUnqualified = s =>
  switch (Str.search_forward(includeUnqualifiedRe, s, 0)) {
  | exception Not_found => false
  | i => true
  };

let origDuneHasUnqualified = hasUnqualified(duneConfig);

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

let extractExtends = s =>
  switch (Str.search_forward(extendsRe, s, 0)) {
  | exception Not_found => None
  | i =>
    let _matched = Str.string_match(extendsRe, s, i);
    Some(Str.matched_group(1, s));
  };

let extractDirsDirective = s =>
  switch (Str.search_forward(dirsRe, s, 0)) {
  | exception Not_found => None
  | i =>
    let _matched = Str.string_match(dirsRe, s, i);
    Some(Str.matched_group(1, s));
  };

let publicName =
  switch (extractPublicName(duneConfig)) {
  | None => "nopublicname"
  | Some(pn) => pn
  };

let extendsString =
  switch (extractExtends(duneConfig)) {
  | None => "nopublicname"
  | Some(pn) => pn
  };

let extends = extractExtends(duneConfig);

let doExtends = extends => {
  let alreadyInTarget = {
    let alreadyInTarget_ = Hashtbl.create(7);
    let onNode = (queryResult: Fs.queryResult, cont) => {
      switch (queryResult) {
      | File(path, stats) =>
        Hashtbl.add(
          alreadyInTarget_,
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
    alreadyInTarget_;
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
          switch (Fs.readText(Fp.append(sibling, "dune"))) {
          | Error(_) => absolutePathsToCopy
          | Ok(lines) =>
            let txt = String.concat("\n", lines);
            switch (extractPublicName(txt)) {
            | None => absolutePathsToCopy
            | Some(pn) =>
              if (String.equal(pn, extends)) {
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
          };
        },
      [],
      siblingDirs,
    );

  let copyFileToTopLevelOfLibraryRule = ((otherLibRoot, absSource)) => {
    let relFromLib = Fp.relativizeExn(~source=duneDir, ~dest=absSource);
    "(copy_files# " ++ Fp.toDebugString(relFromLib) ++ ")";
  };
  let copyLines =
    List.map(copyFileToTopLevelOfLibraryRule, absolutePathsToCopy);
  let copyLinesInternal =
    List.map(copyFileToTopLevelOfLibraryRule, pathsToCopyFromInside);
  let copyString =
    switch (copyLines) {
    | [] =>
      if (String.equal(extends, publicName)) {
        "\n(library-named---"
        ++ publicName
        ++ "---claims-to-implement-itself--That-does-not-make-sense)";
      } else {
        "\n(i-cannot-find-the-virtual-library---"
        ++ extends
        ++ "---supposedly-implemented-by---"
        ++ publicName
        ++ "---make-sure-it-is-a-sibling)";
      }
    | [_, ..._] =>
      String.concat("\n", ["", ...copyLines @ copyLinesInternal])
    };

  let duneConfig = stripExtends(duneConfig, extendsString);

  let duneConfig = Str.global_replace(includeUnqualifiedRe, "", duneConfig);

  let everythingButDebug = duneConfig ++ copyString;
  /* let publicNameDebug = Str.global_replace(dotRe, "--", publicName); */
  /* let debugLines = [ */
  /*   "", */
  /*   "; DEBUG COMMAND", */
  /*   "(alias (name debug-" */
  /*   ++ publicNameDebug */
  /*   ++ ") (action (echo \"" */
  /*   ++ everythingButDebug */
  /*   ++ "\")", */
  /*   "))", */
  /* ]; */
  /* let debugLines = */
  /*   String.split_on_char('\n', String.concat("\n", debugLines)) */
  /*   |> List.map(s => "        " ++ s); */

  /* let debugString = String.concat("\n", debugLines); */
  /* This was changed in dune 2.0 */
  let debugString = "";

  print_endline(everythingButDebug);
  print_endline(debugString ++ "\n");
  exit(0);
};

let doBase = () => {
  print_endline(
    "; Abstract moodule - can't be instantiated but can be extended",
  );
  switch (extractDirsDirective(duneConfig)) {
  | Some(dirs) => print_endline("(dirs " ++ dirs ++ ")")
  | None => ()
  };
  if (origDuneHasUnqualified) {
    print_endline("(include_subdirs unqualified)");
  };
  exit(0);
};

switch (extends) {
/* Then duneConfig *is* the "base class". Only questions are what directories
 * should be considered. We'll say all base classes are abstract and not
 * instantiable for now.  */
| None => doBase()
| Some(extends) => doExtends(extends)
};
