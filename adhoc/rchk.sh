rm "$HOME/Dropbox/projects/filearray_0.1.0.9000.tar.gz"
docker run -v "$HOME/Dropbox/projects":/projects rchk "/projects/filearray_0.1.0.9000.tar.gz"
