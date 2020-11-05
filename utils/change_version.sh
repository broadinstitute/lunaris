old=$1
new=$2

if [[ -z ${old} || -z ${new} ]]; then
  echo "Need to provide old and new versions as arguments."
  exit 1
fi

sed -i "s/Lunaris $old/Lunaris $new/g" docker/Dockerfile
sed -i "s/git checkout v$old/git checkout v$new/g" docker/Dockerfile
sed -i "s/lunaris_${old}_all.deb/lunaris_${new}_all.deb/g" docker/Dockerfile
sed -i "s/Lunaris $old/Lunaris $new/g" vep/docker/Dockerfile
sed -i "s/git checkout v$old/git checkout v$new/g" vep/docker/Dockerfile
sed -i "s/lunaris_${old}_all.deb/lunaris_${new}_all.deb/g" vep/docker/Dockerfile
sed -i "s/Version $old/Version $new/g" src/main/resources/web/lunaris.html
sed -i "s/Version $old/Version $new/g" src/main/resources/web/vep.html
sed -i "s/\"$old\"/\"$new\"/g" src/main/scala/lunaris/app/LunarisInfo.scala
sed -i "s/val lunarisV = \"$old\"/val lunarisV = \"$new\"/g" build.sbt
