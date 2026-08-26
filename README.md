1. It's beta version! No warranties!.. as is .. etc.
2. The Form designer is not working. Just the object templates can be inserted. You can edit existing templates.

I wrote this editor for working with big or middle projects where text navigations is the problem.
I use this editor some months and many bugs was fixed, but many - not.
Please e-mail me bug reports and suggestions.

With best regards
Andrew Shelkovenko. diakin@narod.ru
Jan 2005.

Special thanks for Danny Jackson for CodeEdit semi-visual editor. 
http://groups.yahoo.com/group/rapidq_works/files/CodeEdit/CodeEdit21.zip

Main features:
- multiwindow (pseudo ;-). =======================================================================
You can open 32 files and switch to one using "Windows" menu item.
You can switch to last window (with saving cursor's position) using "LastW" menu item.

Note!!
When you switch windows, RQIDE  refresh Subroutine list. In this time some menu items are 
disabled. 

- named bookmarks. ===============================================================================
You can set 32 bookmarks in every window (opened file)
Use "Bookmarks" menu item. You can "Add", "Delete", "Set", "Sort" bookmarks.
"Add" - adding current line in editor to bookmarks list.
"Delete" - deleting current bookmark from bookmarks list.
"Set" - replace current bookmark by current line. 
"Sort" - sort bookmarks by line number.
Sorting have bug ;(

You can switch to last bookmark  using "LastBM" menu item.

- Projects =======================================================================================
You can save list of Windows (opened files) and bookmarks as Project.
List of projects saved in RQDB.ini file
Project's data saved in *.prj files in "Projects" directory
When RQ Debugger IDE is running, it loads all Windows (and other settings) from last project.
It automatically open last Window and set cursor to saved last position.

You can create new project for working with another Windows (opened files) and bookmarks collection.
Use "File"-"New Project". In opened window set Project name and choose main module from filelist.
Main module is any file with source code and .bas or .rqb extension. 
Then you need to save project. "File"-"Save Project"
Or "Save Project as.."  to save current Windows/bookmarks collection.

To choose other project use "Projects" menu item.

- Directories ======================================================================================
You need to set directories for RapidQ compiler, libraries etc.
Use "Options"-"Directories" menu item. Directories will be saved in *.prj files.

- HiLighting =======================================================================================
You can disable HiLighting. Use "Options"-"Editor"
Disabling is not working in this version!!!

- Components bar ===================================================================================
You can insert component template in cursor position using Components bar.
Component templates are stored in "Templares" subdirectory.
You can edit existing templates. 

- Compile and Run ==================================================================================
Use "Run"-"Compile and Run"  menu item to ..hmm.. Compile source file and Run programm.
Current (opened in window) file will be compiled. 
Other menu item in "Run" menu are not working.

You can compile RapidQ sources - to exe, and Free Basic sources - to exe and dll files.
Use "FB" button to compile exe and "DLL" button - to compile dll.

- Subs\Function list ===============================================================================
Use "View"-"Suroutine list" to View Suroutine and objects list.

----------------------------------------------------------------------------------------------------

Also - see popup menu on source code editor
---------
"Debug (LogEdit)" - (for RapidQ only)
Add debug string that print selected variable name (with index parsing) and value in 
LogEdit (if "dim Logedit as QRichEdit" present in your programm ;))))

for example
1. Select EditMenuItemPop(8).Caption in source text.
2. Use "Debug (LogEdit)" menu item
String will be added

call  AddClrString ("25:EditMenuItemPop("+str$(8)+"Caption )="+(EditMenuItemPop(8).Caption ), clred, LogEdit)
You can change clRed to other color.
--------
"Debug (print)"
The same, but print debug string in console.
Use it if there are not LogEdit in your programm.
--------
"Add Sub"
(Selected text is SubName)

Add sub declaration in "declarations section" (or to begin of text) and add sub template to end of text.
"Declarations section" is next string
'--- Declarations ---
(You can insert it in any line as you wish.)
Sub declaration will be added in next line after this.
Template is
'***************
Sub SubName

end sub


-------
"GoTo Declaration"
"GoTo Sub/Function"
not working
-------
"Open File"
Try to open file using current selection in text editor as file name
----------------------------------------------------


StepByStep Debugger. (For Rapid-Q Basic only!)


=== How it works. ==================================
See StepByStepDebug.bas demo.

Four additional files  creates:
$DbgDisplay.bas - source file with debugger subs
$SrcDisplay.bas - debugged sub\function source for hilighting current operator.
$~OpLen.dat - Lenth of operators for hilighting in $SrcDisplay.bas
$~OpPos.dat - Position of operators for hilighting in $SrcDisplay.bas

Debugger subs.

RQDebug("~$#SubName.varname",varname,0)
            ^^sub name  ^^ var name
Outputs like
print "SubName.varname"=varname

=== How to use debugger
1. Select (in source editor window) variable that you want to watch .
2. Click on "AddWatch" button in debug panel (or in popup menu)
3. Also do that for all variables that you need to watch...
4. Then click on "Compile and run with debug" button in debug panel
5. Your programm will be compiled and runned. When debugged sub\function will be called - debug window
will appears. 
6. Use "Trace" button for step by step debugging and "Continue" button for running.


=== Known bugs and Limitations:
1. RQDebug() not inserted correctly in some cases. 
2. Some bugs with LineNumber


+++++++++++++++++++++++++++++++++++++++++++++++++++
History
+++++++++++++++++++++++++++++++++++++++++++++++++++


16-07-2005  b.347
+ Program loading faster. Not set InitialDir property in QDirTree. 3-4 sec delay.
- bug with saving changes in source editor when fileManager panel opened.
+ saving changes  in files opened vs fileManager

-----------------------------------------------------
15-07-2005 b.339
+ Inc filelist creates faster

-----------------------------------------------------
13-07-2005 b.332
-bug with debug compiling

-----------------------------------------------------
12-07-2005 b.331
+ disable/enable commented lines in FindList
+ return to last window when FileManager closed
+ run exe files by FileManager
- bug with project creating      (Damn it all!!!)

-----------------------------------------------------
12.07.05 b.323
- bug with project deleting

-----------------------------------------------------
22-06-2005 b.320
-bug with text reformatting For...Next 

-----------------------------------------------------
20.06.05  b.319
+ HotTabs hiLight disable option (checkbox)
+ forbid user to setfocus on LineNumber window (richeditor) and roll the number window alone ...
Thanks to Jacques Philippe for his suggestions.

-----------------------------------------------------
19.06.05  b.317
+ English and Russian help files v 0.1

-----------------------------------------------------
17.06.05  b.315 
- serious bug. Now again can create sub\function list properly.

-----------------------------------------------------
16.06.05  b.314 
- serious bug. Now again can compile with debug.

-----------------------------------------------------
12.06.05  b.312 
+ Next\Previous Sub navigations
+ Show current sub name in Main Form caption
+- some bugs

-----------------------------------------------------
08.06.05  b.305 
- disabled unused menu buton
+ Sub/Function List creates more faster.
+ gotoLine in Search panel
+ Menu Item - "Format text"
"Delete Led Spaces" 
"Delete HotTabs"
"ReFormat with HotTabs indents"
"Delete Comments"
"Delete Empty Lines"
- bug BookMarks menu item 

-----------------------------------------------------
05.06.05  b.299 
+ prompt to save changes when exit or load another project.

-----------------------------------------------------
04.06.05  b.297 
- bug with LastW menu item. 

-----------------------------------------------------
RQ Debugger IDE b.296 
02.06.05
- bug with IncFiles menu item.

-----------------------------------------------------
RQ Debugger IDE b.291 
01.06.05
+ command line parameters, saved in Project
+ Menu item "Insert current file name", "Insert current date"
+ IncFiles menu item. Creates list of $Include'd files for quick view\edit. 
+ Code templates (For..next etc). See Templates tab 
+ Sub/Function List creates faster
+ List option for searching. Creates list of lines with founded patterns.
For example all lines where "Tag" pattern presents.
+ file manager. For quick search/view/load files

-----------------------------------------------------
RQ Debugger IDE b.261 
09.05.05
+ works now on Windows XP
+ set cursor to error line when FreeBasic source compiling
- bug with WholeWord search
- bug with last line number
+ command line parameters editor
+- some bugs.

-----------------------------------------------------
RQ Debugger IDE b.246
+ Mouse Wheel support in source editor

-----------------------------------------------------
RQ Debugger IDE v.04b b.245
-bug Project menu

-----------------------------------------------------
RQ Debugger IDE v.04b b.242
-bug Line number

-----------------------------------------------------
RQ Debugger IDE v.04b b.240
21.04.2005
+ Parsedim function, ParseDef function
+ Improve Debug function.
+ debugWindow2.inc
+ ReplaceComment in Parse.inc
+ FreeBasic support (exe and dll)
+ fast HighLight.Dll 
