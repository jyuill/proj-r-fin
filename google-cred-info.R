## google cred - needed for each computer

## 1. Google Cloud Platform > get .json key file from Google Cloud Platform
## 2. Create auth folder in project
## 3. Download/save in auth/ folder
## 4. Create .Renviron file at project root
##    note: can also store in auth/ folder within a folder - like for shiny project
## 5. Add line to .Renviron file: GOOGLE_AUTH_JSON=auth/actual-file-name.json
## 6. Restart RStudio to load .Renviron file
## 7. Note: Add .Renviron to .gitignore so it is NOT pushed up to GitHub repo
## 8. Test with: Sys.getenv('GOOGLE_AUTH_JSON')
## 9. Use in code: (similar to below)
#  if(Sys.getenv("GOOGLE_AUTH_JSON")!=""){
#     gs4_auth(path = paste0("../",Sys.getenv("GOOGLE_AUTH_JSON")))
#  } else {
#.   stop("Please set up google auth json file")
#  }

## IMPORTANT !!! :
## 10. ADD auth/ (and .Renviron) to .gitignore so it is NOT pushed up to GitHub repo 
##    - EASY: Git > select file > gear > ignore (automatically adds to .gitignore)
## (which is why you need to get new key file for each computer to store on hard drive only)