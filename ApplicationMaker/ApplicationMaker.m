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


(* ::Section:: *)
(*Initialization*)


(* ::Subsubsection::Closed:: *)
(*Public Context*)


BeginPackage["ApplicationMaker`ApplicationMaker`"];


Unprotect[NewApplication, BuildApplication, DeployApplication];
ClearAll[NewApplication, BuildApplication, DeployApplication];


(* ::Subsection:: *)
(*Messages*)


(* ::Subsubsection:: *)
(*Usage Messages*)


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


(* ::Subsubsection:: *)
(*Error Messages*)


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


(* ::Subsection::Closed:: *)
(*Private Context*)


Begin["`Private`"];


(* ::Section:: *)
(*Implementation*)


(* ::Subsection:: *)
(*Dependencies*)


(* ::Subsubsection::Closed:: *)
(*One constant*)


$DefaultAppsDir = FileNameJoin@{$UserBaseDirectory, "Applications"};


(* ::Subsubsection::Closed:: *)
(*Subsidiary string operations*)


spaceRiffle = Riffle[Flatten@{##}, " "]&;


spaceRiffledString = Composition[StringJoin, spaceRiffle];


prefixStringViaSpace@s_String := spaceRiffledString@{s, #}&


(* ::Text:: *)
(*I suspect Capitalize was actually a huge mistake of mine. When I put it there I didn't know Linux directories are case sensitive.*)


Capitalize@s_String :=
If[s =!= ""
,  ToUpperCase@StringTake[s, {1}] <> StringDrop[s, {1}]
,  s]


(* ::Text:: *)
(*I hope the following two will become obsolete:*)


fileBaseNameWithoutFirstThreeChars@path_ :=
StringDrop[FileBaseName@path, 3]


dropLastCharsAndAddNewLine@n_Integer?Positive :=
Composition[# <> "\n"&, StringDrop[#, n]&]


(* ::Subsubsection::Closed:: *)
(*Messages styles*)


boldMsg = Style[#, "MSG", Bold]&;
grayMsg = Style[#, "MSG", Gray]&;
blueMsg = Style[#, "MSG", Blue]&;
plainMsg = Style[#, "MSG"]&;


(* ::Subsubsection::Closed:: *)
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
  { "exists" ,     "created"      }
, {    #     , CreateDirectory@# } } // Evaluate]]&;


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
(*Delete multiple files in a directory tree*)


deleteFiles[
  dir_String@sub__
, root:_String:$HomeDirectory] :=
With[{newroot = FileNameJoin@{root, dir}}
, OpenerView @
  { newroot
  , deleteFiles[#, newroot]& /@ {sub} // Column }]

deleteFiles[
  fileNamePattern_String
, root:_String:$HomeDirectory] :=
With[
  { listOfFilesToDelete =
    FileNames @ FileNameJoin @ {root, fileNamePattern}}
, Column @
  { "Files deleted:"
  , Replace[
      (DeleteFile@#; Column@#)& @ listOfFilesToDelete
    , Column@{} -> "(None)"]}]


(* ::Subsection:: *)
(*Main functions*)


(* ::Subsubsection::Closed:: *)
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
, Module[{nb},
    nb = NotebookOpen@path;

    NotebookSave[nb, newpath];

    SetOptions[nb
    , Saveable -> False
    , StyleDefinitions -> FrontEnd`FileName[{"Wolfram"}, "Reference.nb"]
    , DockedCells -> FEPrivate`FrontEndResource["FEExpressions", "HelpViewerToolbar"]
    , PageFooters ->
      { {                None,                  None, None}
      , {Cell[TextData@{footer}, "PageFooter"], None, None} },
      PageHeaders ->
      { {None, None,   None  }
      , {None, None, someCell} }
    , ##&@@Options[nb, WindowTitle]];
    NotebookSave@nb;
    NotebookClose@nb;
    DocumentationSearch`AddDocumentationNotebook[index, newpath];
    fileBaseNameWithoutFirstThreeChars@path]]


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
  ,  Message[BuildApplication::nodir, appName, appDir]; Return@$Failed];

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
  pkg = FileBaseName /@ FileNames[appNameDir <> "/*.m"];

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
      , boldMsg@fileBaseNameWithoutFirstThreeChars@#];
      StringJoin[
        "\t\t\t\t\""
      , FileNameJoin[
        { "Guides"
        , ChangeNotebookSettings[#, index, header, footer] },
          OperatingSystem-> "Unix"]
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
      , boldMsg@fileBaseNameWithoutFirstThreeChars@#];
      StringJoin[
        "\t\t\t\t\""
      , FileNameJoin [
        { "Tutorials"
        , ChangeNotebookSettings[#, index, header, footer] },
          OperatingSystem-> "Unix"
       ]
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
      , boldMsg@fileBaseNameWithoutFirstThreeChars@#];
      StringJoin[
        "\t\t\t\t\""
      , FileNameJoin [
        { "ReferencePages"
        , "Symbols"
        , ChangeNotebookSettings[#, index, header, footer] },
         OperatingSystem-> "Unix"
       ]
      , "\",\n"] ) & /@
    files]];

  WriteString[pacFile, "\t\t\t}
		}
	}
]\n"];
  Close@pacFile;

  DocumentationSearch`CloseDocumentationNotebookIndexer@index;
  PacletManager`RestartPacletManager[];]


(* ::Subsubsection:: *)
(*DeployApplication*)


DeployApplication@args___ := (Message[DeployApplication::argerr]; $Failed)

DeployApplication[
  appName_String
, destDir_String
, appDir_String: $DefaultAppsDir] :=
With[
  { deploymentDirectory = FileNameJoin@{destDir, appName}
  ,          appNameDir = FileNameJoin@{ appDir, appName} },
If[!DirectoryQ@appNameDir
,  Message[BuildApplication::nodir, appName, appDir]; Return@$Failed];

If[MatchQ[CopyDirectory[appNameDir, deploymentDirectory], $Failed]
,  Return@$Failed];

appName[
  "*.nb"
, "Documentation"[
    "English"[
      "ReferencePages"@"Symbols"@#
    , "Guides"@#
    , "Tutorials"@#]]]& @ "___*.nb" // deleteFiles[#, destDir]& ;

Print[
      plainMsg @ "The application ", 
       boldMsg @ appName, 
      plainMsg @ " has been deployed to ", 
       boldMsg @ deploymentDirectory];]


(* ::Section::Closed:: *)
(*Finalization*)


End[]; (* `Private` *)


Protect[NewApplication, BuildApplication, DeployApplication];


EndPackage[]; (* ApplicationMaker` *)
