# rm "$HOME/Dropbox/projects/filearray_0.1.3.9001.tar.gz"
docker run -v "$HOME/Dropbox/projects":/projects kalibera/rchk:latest "/projects/filearray_0.1.3.9001.tar.gz"
