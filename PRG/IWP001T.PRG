*)-H 様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様
*)-H 陳陳陳陳陳陳陳陳� Sys.Info 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*)-H 様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様
*)-H Programa        : IWP001T.PRG -> brow T.documentos e impositivos
*)-H Release		 : 02
*)-H Fecha de inicio : 22/12/1998.-
*)-H Fecha de fin    : 29/12/1998.-
*)-H
*)-H Actualizaciones
*)-H
*)-H  Fecha       Programmer      Comentarios
*)-H 様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様
*)-H 様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様
PARAMETERS w_pref,w_call

PRIVATE ALL LIKE w_*
if empty(parameters())
	w_call=.f.
	w_pref=''
endif

*) Salvado de estados anteriores-----------------------------------------------

push key clear			&&(Salva estados de teclas)
push menu _msysmenu	&&(Salva estado de menu)
set skip of menu _msysmenu .t.

*) Variables internas----------------------------------------------------------

w_prog=upper(prompt())
w_title=ftable(w_pref,'title')
w_pfile='01t'	&& R.02 Mariano

*)-----------------------------------------------------------------------------

if w_call
    ***vfp***
*	do brow01t
    if version()='Visual FoxPro'
		*** cambio el lastkey() ***
		keyboard '{tab}'
		=inkey()
		DO wbrow WITH 'brow01t()'
    else
   		do brow01t
    endif
    ***vfp***
endif

pop key 
pop menu _msysmenu
return

*)様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROC brow01t
*)様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�

sele f01t

IF tipo = "TD"
*			SET FILTER TO tipo = "TD" AND (ALLT(clav)$p_tdoc)	&& R.01 Mariano
			SET FILTER TO tipo = "TD"	&& R.01 Mariano
ELSE
	set filter to tipo=w_pref
ENDIF

browse fields;
	clav :h='C�digo'	  	:p='@K':v=fenter():f,;
	desc :h='Descripci�n':p='@K';
	nome noap noed node norm in screen title (w_title) font 'arial',10;
	valid :f salebrow(Lastkey())

set filt to

***vfp***
IF version()='Visual FoxPro'
	if w_call
		rele wind wbrow
	endif
endif
***vfp***

return

&& R.02b Mariano
*)様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROC valid01t
*)様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
parameters w_show
return
&& R.02e Mariano
