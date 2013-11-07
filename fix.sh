#!/bin/bash          
env GIT_SSL_NO_VERIFY=true git push https://github.com/shainen/nb.git
git add .
git commit -m "fix"
env GIT_SSL_NO_VERIFY=true git push https://github.com/shainen/nb.git