systemctl stop prob-server

cp ./prob /usr/bin/prob
cp ./prob /usr/local/bin/prob

systemctl daemon-reload
systemctl start prob-server
