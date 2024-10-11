@echo off
copy %1 C:\Users\igarashi.DESKTOP-70B58P0\AppData\Local\Temp\temp.md
C:\Users\igarashi.DESKTOP-70B58P0\AppData\Local\Volta\bin\md-to-pdf.exe --config-file C:\Users\igarashi.DESKTOP-70B58P0\AppData\Roaming\.emacs.d\md2pdf.js C:\Users\igarashi.DESKTOP-70B58P0\AppData\Local\Temp\temp.md
rundll32 url.dll,FileProtocolHandler C:\Users\igarashi.DESKTOP-70B58P0\AppData\Local\Temp\temp.pdf
