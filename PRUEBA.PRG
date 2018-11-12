STORE 'abcdefghijklm' TO str1
STORE '12345' TO str2
?str1
?str2
? STUFF(str1, 4, 0, str2)			&& insert
? STUFF(str1, 4, 3, str2)			&& replace
? STUFF(str1, 4, 6, '')				&& delete
? STUFF(str1, 4, 1, str2) 			&& replace and insert
? STUFF(str1, 4, 4, str2)			&& replace and delete
? STUFF(str1, 4, LEN(str1), str2)	&& replace and delete rest