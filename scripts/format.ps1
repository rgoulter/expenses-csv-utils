$files = (Get-ChildItem -Path src -Recurse *.hs | Select-Object -Expand FullName)
dos2unix $files
brittany --write-mode=inplace $files
unix2dos $files
