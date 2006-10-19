//
// Sourcecode from http://www.delphi-library.de/topic_47880.html
//
uses Windows, Messages;

const
  FFM_INIT               = WM_USER + 1976;
  FFM_ONFILEFOUND        = WM_USER + 1974; // wParam: not used, lParam: Filename
  FFM_ONDIRFOUND         = WM_USER + 1975; // wParam: NumFolder, lParam: Directory
var
  CntFolders             : Cardinal = 0;
  NumFolder              : Cardinal = 0;


////////////////////////////////////////////////////////////////////////////////
//
//  FindAllFilesInit
//
//
procedure FindAllFilesInit;
begin
  CntFolders := 0;
  NumFolder := 0;
end;

////////////////////////////////////////////////////////////////////////////////
//
//  CountFolders
//
//
procedure CountFolders(Handle: THandle; RootFolder: string; Recurse: Boolean = True);
var
  hFindFile              : THandle;
  wfd                    : TWin32FindData;
begin
  SendMessage(Handle, FFM_INIT, 0, 0);
  if RootFolder[length(RootFolder)] <> '\' then
    RootFolder := RootFolder + '\';
  ZeroMemory(@wfd, sizeof(wfd));
  wfd.dwFileAttributes := FILE_ATTRIBUTE_NORMAL;
  if Recurse then
  begin
    hFindFile := FindFirstFile(pointer(RootFolder + '*.*'), wfd);
    if hFindFile <> 0 then
    try
      repeat
        if wfd.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY = FILE_ATTRIBUTE_DIRECTORY then
        begin
          if (string(wfd.cFileName) <> '.') and (string(wfd.cFileName) <> '..') then
          begin
            CountFolders(Handle, RootFolder + wfd.cFileName, Recurse);
          end;
        end;
      until FindNextFile(hFindFile, wfd) = False;
      Inc(CntFolders);
    finally
      Windows.FindClose(hFindFile);
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//
//  FindAllFiles
//
procedure FindAllFiles(Handle: THandle; RootFolder: string; Mask: string; Recurse: Boolean = True);
var
  hFindFile              : THandle;
  wfd                    : TWin32FindData;
begin
  if RootFolder[length(RootFolder)] <> '\' then
    RootFolder := RootFolder + '\';
  ZeroMemory(@wfd, sizeof(wfd));
  wfd.dwFileAttributes := FILE_ATTRIBUTE_NORMAL;
  if Recurse then
  begin
    hFindFile := FindFirstFile(pointer(RootFolder + '*.*'), wfd);
    if hFindFile <> 0 then
    try
      repeat
        if wfd.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY = FILE_ATTRIBUTE_DIRECTORY then
        begin
          if (string(wfd.cFileName) <> '.') and (string(wfd.cFileName) <> '..') then
          begin
            FindAllFiles(Handle, RootFolder + wfd.cFileName, Mask, Recurse);
          end;
        end;
      until FindNextFile(hFindFile, wfd) = False;
      Inc(NumFolder);
      SendMessage(Handle, FFM_ONDIRFOUND, NumFolder, lParam(string(RootFolder)));
    finally
      Windows.FindClose(hFindFile);
    end;
  end;
  hFindFile := FindFirstFile(pointer(RootFolder + Mask), wfd);
  if hFindFile <> INVALID_HANDLE_VALUE then
  try
    repeat
      if (wfd.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY <> FILE_ATTRIBUTE_DIRECTORY) then
      begin
        SendMessage(Handle, FFM_ONFILEFOUND, 0, lParam(string(RootFolder + wfd.cFileName)));
      end;
    until FindNextFile(hFindFile, wfd) = False;
  finally
    Windows.FindClose(hFindFile);
  end;
end;
