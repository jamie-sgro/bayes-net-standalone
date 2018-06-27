Option Explicit

'Macro Subroutine
Main_Subroutine

sub Main_Subroutine
	dim path
	path = BrowseForFile()

	if path = "" then
		exit sub
	end if

	dim fileArray, fileName, fileDirectory

	fileArray = Split(path, "\")
	fileName = FileArray(UBound(FileArray))
	fileDirectory = Left(Path, Len(Path) - Len(FileName))
	
	'Check valid file extension
	dim extensionArray, fileExtension
	extensionArray = Split(fileName, ".")
	fileExtension = extensionArray(UBound(extensionArray))
	fileExtension = UCase(fileExtension)
	
	Select Case fileExtension
	Case "CSV"
		writePathToFile(fileArray)
		
		'### This .vbs file no longer executes rScript through chrome portable
		'### Uses .bat to open default browers instead
		
		'Randomize
		'CreateObject("Wscript.Shell").Run "R-Portable\App\R-Portable\bin\R.exe CMD BATCH --vanilla --slave runShinyApp.R" & " " & RND & " ", 0, False
		'CreateObject("Wscript.Shell").Run "R-Portable\App\R-Portable\bin\R.exe CMD BATCH --vanilla --slave runShinyApp.R" & " " & "consoleLog.txt" & " ", 0, False
	Case "XLSX"
		fileExtError(fileExtension)
	Case "XLSM"
		fileExtError(fileExtension)
	Case "XLSB"
		fileExtError(fileExtension)
	Case "XLS"
		fileExtError(fileExtension)
	Case "XML"
		fileExtError(fileExtension)
	Case "TXT"
		fileExtError(fileExtension)
	Case Else
		Err.Raise 1002, "File Extension Error", "Could not parse a valid file extension"
	End Select
	
	exit sub
end sub

function BrowseForFile()
    With CreateObject("WScript.Shell")
        Dim fso : Set fso = CreateObject("Scripting.FileSystemObject")
        Dim tempFolder : Set tempFolder = fso.GetSpecialFolder(2)
        Dim tempName : tempName = fso.GetTempName() & ".hta"
        Dim path : path = "HKCU\Volatile Environment\MsgResp"
        With tempFolder.CreateTextFile(tempName)
            .Write "<input type=file name=f>" & _
            "<script>f.click();(new ActiveXObject('WScript.Shell'))" & _
            ".RegWrite('HKCU\\Volatile Environment\\MsgResp', f.value);" & _
            "close();</script>"
            .Close
        End With
        .Run tempFolder & "\" & tempName, 1, True
        BrowseForFile = .RegRead(path)
        .RegDelete path
        fso.DeleteFile tempFolder & "\" & tempName
    End With
end function

function writePathToFile(fileArray)
	dim wd, rootDir
	wd = CreateObject("Scripting.FileSystemObject").GetParentFolderName(WScript.ScriptFullName)
	
	dim fso, LogPath, LogFile
	
	'Index folder path and redirect to deploy fsoPath next to .R source code
	rootDir = inStr(wd, "\dist")
	rootDir = left(wd, rootDir)
	
	LogPath = rootDir & "app\shiny\fsoPath.dat"
	
	Set fso = CreateObject("Scripting.FileSystemObject")
	
	If NOT fso.FolderExists(rootDir & "app\") then
		Err.Raise 1003, "File Pathing Error", "app\ folder path cannot be found."
	End if
	
	If NOT fso.FolderExists(rootDir & "app\shiny") then
		Err.Raise 1003, "File Pathing Error", "app\shiny\ folder path cannot be found."
	End if
	
	If NOT fso.FileExists(LogPath) then
		Set LogFile = fso.CreateTextFile(LogPath,False)
		Set LogFile = Nothing
	End if
	
	Set LogFile = fso.OpenTextFile(LogPath,2)
	
	dim rPath, i
	
	rPath = fileArray(0)
	
	for i = 1 to uBound(fileArray)
		rPath = rPath & "/" & fileArray(i)
	next

	LogFile.WriteLine("filePath = '" & rPath & "'")
	LogFile.WriteLine("")
	LogFile.Close
end function

function fileExtError(ext)
	Err.Raise 1001, "File Extension Error", "This version is not compatable with ." & ext & " files"
end function