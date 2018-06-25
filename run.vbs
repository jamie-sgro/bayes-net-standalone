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
		
		Randomize
		'CreateObject("Wscript.Shell").Run "R-Portable\App\R-Portable\bin\R.exe CMD BATCH --vanilla --slave runShinyApp.R" & " " & RND & " ", 0, False
		CreateObject("Wscript.Shell").Run "R-Portable\App\R-Portable\bin\R.exe CMD BATCH --vanilla --slave runShinyApp.R" & " " & "consoleLog.txt" & " ", 0, False
	Case "XLSX"
		msgbox("This version is not compatable with .xlsx files")
	Case "XLSM"
		msgbox("This version is not compatable with .xlsm files")
	Case "XLSB"
		msgbox("This version is not compatable with .xlsb files")
	Case "XLS"
		msgbox("This version is not compatable with .xls files")
	Case "XML"
		msgbox("This version is not compatable with .xml files")
	Case "TXT"
		msgbox("This version is not compatable with .txt files")
	Case Else
		msgbox("Could not parse a valid file extension")
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
	dim wd
	wd = CreateObject("Scripting.FileSystemObject").GetParentFolderName(WScript.ScriptFullName)
	
	dim fso, LogPath, LogFile
	
	Set fso = CreateObject("Scripting.FileSystemObject")

	If NOT fso.FolderExists(wd & "\Shiny\") then
		Err.Raise 1001, "File Pathing Error", "app.r folder path cannot be found."
	End if
	
	LogPath = wd & "\Shiny\fsoPath.dat"
	
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