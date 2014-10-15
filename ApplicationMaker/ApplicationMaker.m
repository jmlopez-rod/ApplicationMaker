(* ::Package:: *)

(* :Title: ApplicationMaker *) 
(* :Author: jmlopez *)
(* :Email: jmlopez.rod@gmail.com *)
(* :Summary: Package provides functions to create directory tree and to export
			 the application. *)
(* :Context: ApplicationMaker`ApplicationMaker` *)
(* :Package version: 1.0 *)
(* :History:  Version 1.0 July 09 2011 *)
(* :Mathematica version: 8.0 for Mac OS X x86 (64-bit) (February 23, 2011) *)
(* :Discussion: The function NewApplication creates a directory tree at a
				specified location for a new application. The DeployApplication
				takes all the m files and documentation notebooks in the
				application and puts them in an specified location for 
				distribution. *)


(* ::Section::Closed:: *)
(*Initialiazation*)


BeginPackage["ApplicationMaker`ApplicationMaker`"];


Unprotect[NewApplication, BuildApplication, DeployApplication];
ClearAll[NewApplication, BuildApplication, DeployApplication];


(* :Usage Messages: *)


$DefaultAppsDir::usage = "Default directory for Applications developing and deploying.";


NewApplication::usage = 
"NewApplication[\!\(\*
StyleBox[\"appName\", \"TI\"]\)] creates a directory named \!\(\*
StyleBox[\"appName\", \"TI\"]\) in \!\(\*
StyleBox[\"$UserBaseDirectory\", \"Program\"]\)\!\(\*
StyleBox[\"/\", \"Program\"]\)\!\(\*
StyleBox[\"Applications\", \"Program\"]\)\!\(\*
StyleBox[\"/\", \"Program\"]\) and subdirectories required to make an application with documentation.";
BuildApplication::usage =
"BuildApplication[\!\(\*
StyleBox[\"appName\", \"TI\"]\)] creates the documentation files and the index for the documentation center.";
DeployApplication::usage =
"DeployApplication[\!\(\*
StyleBox[\"appName\", \"TI\"]\), \!\(\*
StyleBox[\"destDir\", \"TI\"]\)] copies the m files and documentation of your application into \!\(\*
StyleBox[\"destDir\", \"TI\"]\)";


(* :Error Messages: *)


NewApplication::argerr =
"String specifying the application name was expected.";
BuildApplication::argerr =
"String specifying the application name was expected.";
BuildApplication::nodir =
"There is no application `1` in `2`. To create a new application use NewApplication";
DeployApplication::argerr =
"Strings specifying the application name and the destination were expected.";
DeployApplication::nodir =
"There is no application `1` in `2`. To create a new application use NewApplication";


Begin["`Private`"];


(* ::Section:: *)
(*Implementation*)


(* ::Subsubsection::Closed:: *)
(*One constant*)


$DefaultAppsDir = FileNameJoin@{$UserBaseDirectory, "Applications"};


(* ::Subsubsection::Closed:: *)
(*Subsidiary string operations*)


spaceRiffle = Riffle[Flatten@{##}, " "]&;


spaceRiffledString = Composition[StringJoin, spaceRiffle];


prefixStringViaSpace@s_String := spaceRiffledString@{s, #}&


Capitalize@s_String :=
If[StringLength@s =!= 0
,  ToUpperCase@StringTake[s, {1}] <> StringDrop[s, {1}]
,  s]


(* ::Text:: *)
(*I hope the following two will become obsolete:*)


fileBaseNameWithoutFirstThree@path_ :=
StringDrop[FileBaseName@path, 3]


dropLastCharsAndAddNewLine@n_Integer?Positive :=
Composition[# <> "\n"&, StringDrop[#, n]&]


(* ::Subsubsection::Closed:: *)
(*Messages styles*)


boldMsg = Style[#, "MSG", Bold]&;
grayMsg = Style[#, "MSG", Gray]&;
blueMsg = Style[#, "MSG", Blue]&;
plainMsg = Style[#, "MSG"]&;


(* ::Subsubsection:: *)
(*Directory tree creation*)


(* ::Text:: *)
(*The original commented directory creation (hopefully, obsolete already):*)


createDirWithCommentPrinted =
If[DirectoryQ@#
,  Print[grayMsg@"Directory exists: ", boldMsg@#]
,  CreateDirectory@#;
   Print[blueMsg@"Directory created: ", boldMsg@#]]&;


(* ::Text:: *)
(*Modified commented directory creation:*)


createDirIfPossible =
Block[{CreateDirectory},
If[DirectoryQ@#,
Sequence @@ Transpose @
{ prefixStringViaSpace@"Directory" /@
  {   "exists"    ,     "created"     }
, { Capitalize@# , CreateDirectory@# } } // Evaluate]]&;


createDirTree[address___, dir_String@subDirs___] :=
Map[createDirTree[address, dir, #]&, dir@subDirs, Heads -> True]

createDirTree[address___, _?AtomQ] :=
createDirIfPossible@FileNameJoin@{address}


(* ::Text:: *)
(*Directories are represented as "dirName"[], so\[Ellipsis]*)


openAtoms = Replace[#, a_?AtomQ :> a[], {1, \[Infinity]}]&;


stringTreeQ@expr_ :=
MatchQ[
  Replace[
    Cases[expr, _?AtomQ|_?AtomQ[]
    , {0, \[Infinity]}, Heads -> True]
  , s_String[] :> s, {1}]
, {__String}]


Options@CreateDirectoryTree = {"Output Form" -> OpenerView};

CreateDirectoryTree[ tree_?stringTreeQ
                   , root:_String:$HomeDirectory
                   , OptionsPattern[]] :=
createDirTree@openAtoms@root@tree //.
If[OptionValue@"Output Form" === OpenerView
,  { {msg_String, addr_String}[] :> msg <> ": " <> addr
   , {msg_String, addr_String}@sub__ :>
     OpenerView[{{msg, addr}[], Column@{sub}}, False]}
,  {}]


(* ::Subsubsection:: *)
(*NewApplication*)


(* ::Text:: *)
(*I' m still not sure if this is the directory tree that should be created: couldn't figure this out from the original program. This tree is based on examples of existing apps' dirs.*)


NewApplication@args___ := (Message[NewApplication::argerr]; $Failed)

NewApplication[
  appName_String
, appsDir_String: $DefaultAppsDir] :=
CreateDirectoryTree[
  appName[
    "Kernel"
  , "Documentation"[
      "English"[
        "Guides"
      , "Tutorials"
      , "ReferencePages"@"Symbols"]]]
, appsDir]


ChangeNotebookSettings[path_, index_, header_, footer_] :=
With[
  { someCell =
    Cell[
      TextData @
      { Cell[TextData@{header}, "PageHeader"]
      , Cell[TextData@{CounterBox@"Page"}, "PageNumber"] }
    , CellMargins -> {{Inherited, -29}, {Inherited, Inherited}}]
  , newpath =
    FileNameJoin @
    { DirectoryName@path
    , StringDrop[FileNameTake@path, 3] }}
, Module[{nb, winTitle},
    nb = NotebookOpen@path;

    winTitle = Options[nb, WindowTitle][[1]][[2]];

    NotebookSave[nb, newpath];

    SetOptions[nb,
      Saveable -> False
    , StyleDefinitions -> FrontEnd`FileName[{"Wolfram"}, "Reference.nb"]
    , DockedCells -> FEPrivate`FrontEndResource["FEExpressions", "HelpViewerToolbar"]
    , PageFooters ->
      { {                None,                  None, None}
      , {Cell[TextData@{footer}, "PageFooter"], None, None} },
      PageHeaders ->
      { {None, None,   None  }
      , {None, None, someCell} }
    , WindowTitle -> winTitle];
    NotebookSave@nb;
    NotebookClose@nb;
    DocumentationSearch`AddDocumentationNotebook[index, newpath];
    fileBaseNameWithoutFirstThree@path]]


(* ::Subsubsection::Closed:: *)
(*BuildApplication*)


BuildApplication@args___ := (Message[BuildApplication::argerr];$Failed)

BuildApplication[
  appName_String
, version_String: "0.0.1"
, header_String: ""
, footer_String: ""
, appDir_String: $DefaultAppsDir] :=
Module[
  {appNameDir, indexDir, tmpPath, files, pacFile, pkg, index, str},

  appNameDir = FileNameJoin@{appDir, appName};
  indexDir = FileNameJoin@{appNameDir, "Documentation", "English", "Index"};

  If[!DirectoryQ@appNameDir
  ,  Message[BuildApplication::nodir, appName, appDir]; Return[$Failed]];

  If[FileNames@indexDir != {}
  ,  DeleteDirectory[indexDir, DeleteContents -> True]];

  index = DocumentationSearch`NewDocumentationNotebookIndexer@indexDir;

  pacFile = OpenWrite@FileNameJoin@{appNameDir, "PacletInfo.m"};

  WriteString[pacFile
  , "Paclet[
	Name -> \"" <> appName <> "\",
	Version -> \"" <> version <> "\",
	MathematicaVersion -> \"8+\",
	Extensions -> {
		{
			\"Kernel\",
			\"Context\" -> {\n"];
  pkg = FileBaseName /@ FileNames[appNameDir <> "/*.nb"];

  WriteString[pacFile
  , dropLastCharsAndAddNewLine[3] @
    StringJoin["\t\t\t\t\"" <> appName <> "`" <> # <> "`\", \n" & /@ pkg]];

  WriteString[pacFile, "\t\t\t}
		},
		{
			\"Documentation\",
			Language -> \"English\",
			LinkBase -> \"" <> appName <> "\",
			Resources -> {\n"];

(* :GUIDES: *)

  files =
  FileNames @
  FileNameJoin@{appNameDir
  , "Documentation"
  , "English"
  , "Guides"
  , "___*"};

  WriteString[pacFile
  , StringJoin[
    ( Print[
        grayMsg@"Adding Guide : "
      , boldMsg@fileBaseNameWithoutFirstThree@#];
      StringJoin[
        "\t\t\t\t\""
      , FileNameJoin @
        { "Guides"
        , ChangeNotebookSettings[#, index, header, footer] }
      , "\",\n"] ) & /@
    files]];

(* :TUTORIALS: *)

  files =
  FileNames @
  FileNameJoin@{appNameDir
  , "Documentation"
  , "English"
  , "Tutorials"
  , "___*"};

  WriteString[pacFile
  , StringJoin[
    ( Print[
        grayMsg@"Adding Tutorial : "
      , boldMsg@fileBaseNameWithoutFirstThree@#];
      StringJoin[
        "\t\t\t\t\""
      , FileNameJoin @
        { "Tutorials"
        , ChangeNotebookSettings[#, index, header, footer] }
        , "\",\n"] ) & /@
      files]];

(* :REFERENCES: *)

  files =
  FileNames @
  FileNameJoin@{appNameDir
  , "Documentation"
  , "English"
  , "ReferencePages"
  , "Symbols"
  , "___*"};

  WriteString[pacFile
  , dropLastCharsAndAddNewLine[2] @
    StringJoin[
    ( Print[
        grayMsg@"Adding Reference : "
      , boldMsg@fileBaseNameWithoutFirstThree@#];
      StringJoin[
        "\t\t\t\t\""
      , FileNameJoin @
        { "ReferencePages"
        , "Symbols"
        , ChangeNotebookSettings[#, index, header, footer] }
      , "\",\n"] ) & /@
    files]];

  WriteString[pacFile, "\t\t\t}
		}
	}
]\n"];
  Close@pacFile;

  DocumentationSearch`CloseDocumentationNotebookIndexer@index;
  PacletManager`RestartPacletManager[];]


(* ::Subsubsection::Closed:: *)
(*DeployApplication*)


DeployApplication@args___ := (Message[DeployApplication::argerr]; $Failed)

DeployApplication[
  appName_String
, destDir_String
, appDir_String: $DefaultAppsDir] :=
Module[
  {appNameDir, files},
  appNameDir = FileNameJoin@{appDir, appName};
  If[!DirectoryQ@appNameDir
  ,  Message[BuildApplication::nodir, appName, appDir]; Return@$Failed];

  If[MatchQ[CopyDirectory[appNameDir, FileNameJoin@{destDir, appName}], $Failed]
  ,  Return@$Failed];

  Composition[DeleteFile
  , FileNames, FileNameJoin] /@
  { {destDir, appName, "*.nb"}
  , {destDir, appName, "Documentation", "English", "ReferencePages", "Symbols", "___*.nb"}
  , {destDir, appName, "Documentation", "English", "Guides", "___*.nb"}
  , {destDir, appName, "Documentation", "English", "Tutorials", "___*.nb"} };

  Print[
        plainMsg @ "The application ", 
         boldMsg @ appName, 
        plainMsg @ " has been deployed to ", 
         boldMsg @ FileNameJoin@{destDir, appName}];]


(* ::Section::Closed:: *)
(*Finalization*)


End[];


Protect[NewApplication, BuildApplication, DeployApplication];


EndPackage[];
