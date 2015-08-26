echo "yes" | sudo add-apt-repository ppa:avsm/ppa
sudo apt-get update -qq
sudo apt-get install ocaml ocaml-native-compilers camlp4-extra opam opam

export OPAMYES=1
opam init
opam update
opam install oasis ocamlfind
eval `opam config env`

./configure
make

if [ "$TRAVIS_REPO_SLUG" == "oklm-wsh/Decompress" ] \
     && [ "$TRAVIS_PULL_REQUEST" == "false" ] \
     && [ "$TRAVIS_BRANCH" == "master" ]; then

  echo -e "Publishing ocamldoc...\n"

  git config --global user.email "travis@travis-ci.org"
  git config --global user.name "travis-ci"
  git clone https://${GH_TOKEN}@github.com/oklm-wsh/Decompress .documentation

  cd .documentation
  git fetch
  git checkout gh-pages
  git merge master --commit -m "Merge master into gh-pages"

  ./configure
  make doc

  git add -f doc/

  if [ -n "$(git status --untracked-files=no --porcelain)" ]; then
    git commit -m "Update documentation $TRAVIS_BUILD_NUMBER"
    git push -fq origin gh-pages
  fi

  echo -e "Published ocamldoc to gh-pages.\n"
fi
