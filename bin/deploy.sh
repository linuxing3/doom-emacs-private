# build
cd ~/docs
hugo -D


# make key
mkdir ~/.ssh
cat > ~/.ssh/id_rsa << EOF
$DEPLOY_KEY
EOF
chmod 600 ~/.ssh/id_rsa
cat ~/.ssh/id_rsa

# deploy
cd ~/docs
echo "yes
"|bash <(rsync -avzr --delete --exclude .git/ public/ $USER@$HOST:$DIR)
