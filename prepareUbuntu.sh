apt-get update
apt-get install -y  gnupg curl openjdk-8-jdk gcc zlib1g-dev
echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" |  tee /etc/apt/sources.list.d/sbt.list
echo "deb https://repo.scala-sbt.org/scalasbt/debian /" |  tee /etc/apt/sources.list.d/sbt_old.list
curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" |  apt-key add
 apt-get update
 apt-get install -y sbt
