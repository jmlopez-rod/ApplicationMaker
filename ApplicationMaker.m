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
(*Initialization*)


(* ::Subsection::Closed:: *)
(*Public Context*)


BeginPackage["ApplicationMaker`ApplicationMaker`"];


Unprotect[NewApplication, BuildApplication, DeployApplication];
ClearAll[NewApplication, BuildApplication, DeployApplication];


(* ::Subsection::Closed:: *)
(*Messages*)


(* ::Subsubsection:: *)
(*Usage Messages*)


$ApplicationsDirectory::usage = "Default directory for Applications developing and deploying.";


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


$ApplicationsDirectory = FileNameJoin@{$UserBaseDirectory, "Applications"};


(* ::Subsubsection::Closed:: *)
(*Subsidiary string operations*)


spaceRiffle = Riffle[Flatten@{##}, " "]&;


spaceRiffledString = Composition[StringJoin, spaceRiffle];


prefixStringViaSpace@s_String := spaceRiffledString@{s, #}&


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


(* ::Subsubsection::Closed:: *)
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


(* ::Subsubsection:: *)
(*listOfFiles*)


listOfFiles[
  dir_String@sub__
, root:_String:$HomeDirectory] :=
With[{newroot = FileNameJoin@{root, dir}}
, listOfFiles[#, newroot]& /@ {sub} // Flatten]

listOfFiles[
  fileNamePattern_String
, root:_String:$HomeDirectory] :=
FileNames @ FileNameJoin @ {root, fileNamePattern}


(* ::Subsection:: *)
(*Main functions*)


(* ::Subsubsection::Closed:: *)
(*NewApplication*)


NewApplication[
  appName_String
, appsDir_String: $ApplicationsDirectory] :=
CreateDirectoryTree[
  appName[
    "Kernel"
  , "Documentation"[
      "English"[
        "Guides"
      , "Tutorials"
      , "ReferencePages"@"Symbols"]]]
, appsDir]

NewApplication@smthElse___ := (Message[NewApplication::argerr]; $Failed)


(* ::Subsubsection::Closed:: *)
(*ChangeNotebookSettings*)


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
    DocumentationSearch`AddDocumentationNotebook[index, newpath];]]


(* ::Subsubsection:: *)
(*BuildApplication*)


packageFilesNames[appName_String
, appsDir_String:$ApplicationsDirectory] :=
DeleteCases[
  FileBaseName /@ FileNames@FileNameJoin@{appsDir, appName, "*.m"}
, "PacletInfo"]


contextNames[appName_String
, appsDir_String:$ApplicationsDirectory] :=
appName <> "`" <> # <> "`" & /@
packageFilesNames[appName, appsDir]


englishResourcesList[appName_String
, appsDir_String:$ApplicationsDirectory] :=
Composition[
  FileNameJoin[#, OperatingSystem -> "Unix"]&
, Replace[#
  , {most___, "Documentation", "English", rest___, last_} :>
    {rest, StringReplace[FileBaseName@last
           , StartOfString~~"_"..~~filename_ :> filename]}]&
, FileNameSplit] /@
listOfFiles[
  appName[
    "Documentation"[
      "English"[
        "Guides"@#
      , "Tutorials"@#
      , "ReferencePages"@"Symbols"@#]]& @ "___*.nb"]
, appsDir]


BuildApplication[
  appName_String
, version_String: "0.0.1"
, header_String: ""
, footer_String: ""
, appsDir_String: $ApplicationsDirectory] :=
With[{engResources = englishResourcesList[appName, appsDir]},
Module[
  {appNameDir, indexDir, index},

  appNameDir = FileNameJoin@{appsDir, appName};
  indexDir = FileNameJoin@{appNameDir, "Documentation", "English", "Index"};

  If[!DirectoryQ@appNameDir
  ,  Message[BuildApplication::nodir, appName, appsDir]; Return@$Failed];

  If[FileNames@indexDir != {}
  ,  DeleteDirectory[indexDir, DeleteContents -> True]];

  index = DocumentationSearch`NewDocumentationNotebookIndexer@indexDir;

  Block[
    { Paclet = PacletManager`Paclet
    , Name = Global`Name
    , MathematicaVersion = Global`MathematicaVersion
    , Extensions = Global`Extensions
    , LinkBase = Global`LinkBase
    , Resources = Global`Resources}
  , Export[FileNameJoin@{appNameDir, "PacletInfo.m"}
    , Paclet[Name -> appName
      , Version -> version
      , MathematicaVersion -> "8+"
      , Extensions ->
        { { "Kernel", "Context" -> contextNames[appName, appsDir]}
        , { "Documentation", Language -> "English", LinkBase -> appName
          , Resources -> engResources}}]
    , "Package"]];

  ChangeNotebookSettings[
    FileNameJoin@{appNameDir, "Documentation", "English", # <> ".nb"}
  , index, header, footer]& /@
  engResources;

  DocumentationSearch`CloseDocumentationNotebookIndexer@index;
  PacletManager`RestartPacletManager[];]]

BuildApplication@smthElse___ := (Message[BuildApplication::argerr]; $Failed)


(* ::Subsubsection::Closed:: *)
(*DeployApplication*)


DeployApplication[
  appName_String
, destDir_String
, appsDir_String: $ApplicationsDirectory] :=
With[
  { deploymentDirectory = FileNameJoin@{destDir, appName}
  ,          appNameDir = FileNameJoin@{ appsDir, appName} },
If[!DirectoryQ@appNameDir
,  Message[BuildApplication::nodir, appName, appsDir]; Return@$Failed];

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

DeployApplication@smthElse___ := (Message[DeployApplication::argerr]; $Failed)


(* ::Section::Closed:: *)
(*Finalization*)


End[]; (* `Private` *)


Protect[NewApplication, BuildApplication, DeployApplication];


EndPackage[]; (* ApplicationMaker` *)
