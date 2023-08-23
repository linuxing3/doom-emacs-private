#install hugo
wget -O hugo.deb https://github.com/gohugoio/hugo/releases/download/v0.70.0/hugo_0.70.0_Linux-64bit.deb
sudo dpkg -i hugo.deb
type hugo

# create site
hugo new site docs/site
cp docs/*.org docs/site/content/

# install them
cd docs/site
git init
git submodule add https://github.com/budparr/gohugo-theme-ananke.git themes/ananke
echo 'theme = "ananke"' >>config.toml
sed -i 's/example\.org/xunqinji\.top\/hugo/g' config.toml
echo "Done with settings!"

# build
hugo -D

# deploy
#cd ~/docs
#echo "yes
#"|bash <(rsync -avzr --delete --exclude .git/ public/ $USER@$HOST:$DIR)
