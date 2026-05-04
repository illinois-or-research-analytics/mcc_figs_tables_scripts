#!/usr/bin/expect -f

set credentials [open "/mnt/a/u/sciteam/minhyuk2/.dockerhub_login" r]
gets $credentials username
gets $credentials password
close $credentials

spawn shifterimg login
expect "default username:"
send "$username\r"
expect "default password:"
send -- "$password\r"
expect eof
