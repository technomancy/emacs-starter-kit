#!/bin/ksh
. /bin/shared/.mkt.cfg


STORE_TMP=/tmp/stores.txt
cd /spool/xml

rm -f $STORE_TMP

bteq << EOF
.SESSIONS 1
.LOGON $UserId,$Password;

.EXPORT DATA FILE "$STORE_TMP"
select division_id,store_id,
trim(store)||' '||trim(store_addr_line2_txt)||', '||
trim(store_city)
from stores
where status_id='A'
 and division_id in (517,1920,2445)
order by division_id,store_city,store_id;

.EXIT 0
EOF

perl <<EOF > $1
print qq(<?xml version="1.0" encoding="utf-8"?>\n<stores>);
open IN, '<$STORE_TMP';
while (read IN,\$info, 12) {
 my (\$div, \$s) = unpack 'x2 i i', \$info;  # read binary nums
 \$_ = <IN>;   # Read store name
 chop;     # Remove newline
 s/(\w+)/\u\L\$1/g; # Title case
 s/&/&amp;/g;  # Fix ampersands
 s/"/&quot;/g; # Fix quotes
 printf qq(<s d="%02d" i="%d" n="%s"/>\n),\$div,\$s,\$_;
}
print "</stores>";
EOF

rm $STORE_TMP
