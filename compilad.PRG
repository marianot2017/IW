SET LOGERRORS ON
SET DATE FRENCH
SET CENTURY ON
SET DELETED ON
close data

use k:\iw\iwp000.pjx alias proy

go top
do while !eof()
	if upper(proy.type)='P'
		w_prog=STRTRAN(name,'.prg','')
		if asc(right(w_prog,1))=0
			w_prog=left(w_prog,len(w_prog)-1)
		endif
		compile &w_prog
		WAIT WINDOW w_prog nowait
		if FILE(UPPER(w_prog+'.err'))
			MODIFY file &w_prog..err 
			MODIFY COMMAND &w_prog..prg
			loop
		endif
	endif
	skip 
	IF LASTKEY()=27
		EXIT
	endif
enddo
use
return