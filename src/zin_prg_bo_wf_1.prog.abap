*****           Implementation of object type ZIN_BO_1             *****
INCLUDE <OBJECT>.
BEGIN_DATA OBJECT. " Do not change.. DATA is generated
* only private members may be inserted into structure private
DATA:
" begin of private,
"   to declare private attributes remove comments and
"   insert private attributes here ...
" end of private,
  BEGIN OF KEY,
      KEY_NUMBER LIKE ZIN_S_WF_NUM-NUMBER,
  END OF KEY.
END_DATA OBJECT. " Do not change.. DATA is generated
