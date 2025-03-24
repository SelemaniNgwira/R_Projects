# Run command to download R 4.4.3 installer from the R Project's website
url <- "https://cran.r-project.org/bin/windows/base/R-4.4.3-win.exe"
download.file(url, destfile = "R-4.4.3-win.exe")

# Run the installer to update R
system("R-4.4.3-win.exe /SILENT")  # /SILENT for silent installation (no user prompts)

# Optional: Remove the installer after installation
file.remove("R-4.4.3-win.exe")

# Message after installation
cat("R has been updated to version 4.4.3. Please restart R.")
