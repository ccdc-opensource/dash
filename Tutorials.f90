

	SUBROUTINE LaunchTutorial(Tutorial_ID)
	USE winteracter
	USE druid_header
	USE variables
	CHARACTER*255 DirString, FileDir
	CHARACTER*255 Files(100)
	CHARACTER*255 CurDir
	INTEGER Tutorial_ID
	INTEGER NFiles

	NFiles = 100
	CALL IOsDirName(CurDir)
	IF       (Tutorial_ID .EQ. ID_Tutorial_1) THEN
		DirString = INSTDIR(1:len_trim(INSTDIR))//DIRSPACER//"Documentation"//DIRSPACER//"tutorial1"
	ELSE IF  (Tutorial_ID .EQ. ID_Tutorial_2) THEN
		DirString = INSTDIR(1:len_trim(INSTDIR))//DIRSPACER//"Documentation"//DIRSPACER//"tutorial2"
	ELSE IF  (Tutorial_ID .EQ. ID_Tutorial_3) THEN
		DirString = INSTDIR(1:len_trim(INSTDIR))//DIRSPACER//"Documentation"//DIRSPACER//"tutorial3"
 	ELSE IF  (Tutorial_ID .EQ. ID_Tutorial_4) THEN
		DirString = INSTDIR(1:len_trim(INSTDIR))//DIRSPACER//"Documentation"//DIRSPACER//"tutorial4"
 	ELSE IF  (Tutorial_ID .EQ. ID_Tutorial_5) THEN
		DirString = INSTDIR(1:len_trim(INSTDIR))//DIRSPACER//"Documentation"//DIRSPACER//"tutorial5"
	END IF
	FileDir = DirString(1:len_trim(DirString))//DIRSPACER//"data files"

	CALL IOsDirList(FileDir(1:len_trim(FileDir)),"",Files,Nfiles)

	CALL IOsDirChange(FileDir(1:len_trim(FileDir)))
	DO I = 1, Nfiles
		CALL IOsCopyFile(Files(I)(1:len_trim(Files(I))),CURDIR(1:len_trim(CURDIR))//DIRSPACER)
	END DO
	CALL IOsDirChange(CurDir(1:len_trim(CurDir)))

	CALL WHelpFile(' ') ! In case the help file is open already
	CALL WHelpFile(DirString(1:len_trim(DirString))//DIRSPACER//"index.htm")

	RETURN
	END