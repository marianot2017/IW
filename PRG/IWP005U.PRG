*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP550.PRG -> Reasignacion de jurisdiciones
*)-H Release		 : 01
*)-H Fecha de inicio : 22/12/1998.-
*)-H Fecha de fin    : 29/12/1998.-
*)-H
*)-H Actualizaciones
*)-H
*)-H Fecha       Programmer      Comentarios
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PRIVATE ALL LIKE w_*

*) Salvado de estados anteriores-----------------------------------------------

push key clear		  &&(Salva estados de teclas)
push menu _msysmenu &&(Salva estado de menu)
set skip of menu _msysmenu .t.

*) Variables internas----------------------------------------------------------

w_prog=upper(prompt())
w_alta=.f.

*) Variables del proceso-------------------------------------------------------

w_pfile='05u'
w_title='REASIGNACION DE JURISDICCIONES'

w_pede='/'
w_peha='/'
w_clde=space(5)
w_clha='99999'

*) Apertura de archivos--------------------------------------------------------

fieldvec[1]='F00'
fieldvec[2]='F02'
fieldvec[3]='F04'
fieldvec[4]='F05'

use (d0f00) in 0 alias 'f00'									&&(Parametros)
use (d0f02) in 0 alias 'f02' order 'd2clie' EXCLUSIVE &&(Clientes)
use (d0f04) in 0 alias 'f04' order 'd4peri' EXCLUSIVE &&(Periodos)
use (d0f05) in 0 alias 'f05' order 'd5peri' EXCLUSIVE &&(Comprobantes)

if neterr()
	pop key
	pop menu _msysmenu
	fieldvec=''
	=closall()
	return
endif

fieldvec=''

*)-----------------------------------------------------------------------------

sele f04
go top
w_pede=d4Peri
go bott
w_peha=d4Peri

do ssm998.mpr
do iws005u.spr

pop key
pop menu _msysmenu
=closall()
return

***vfp***
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons05u
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	on key label f9 do cons05u0
	keyboard '{f9}'
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons05u0
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
on key label f9
***vfp***

if !valid05u(.t.)
	wait wind 'Verifique los datos ingresados...'
	return
endif

if sino('Confirma Reasignaci๓n de Jurisdicciones ?.','N')='S'
	if begintran()
		wait wind nowait 'Procesando...'
      sele f05
      repl all d5juri with readf('f02',f05->d5clie,'d2juri',1);
	   	for d5tipo='V' and;
	   	between(d5clie,w_clde,w_clha) and;
	   	between(ptoi(d5peri),ptoi(w_pede),ptoi(w_peha))		&& R.01 Mariano
   	=endtran()
   	wait clear
   	=advsuave('Reasignaci๓n finalizada. acepte para continuar...','Informaci๓n')
   endif
endif
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC valid05u
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_show

if empty(parameters())
	w_show=.f.
endif

PRIVATE w_retu,w_vari
w_retu=.t.
w_vari=varread()

if w_vari='W_PEDE' or w_show
	w_pede=strcero(left(w_pede,2))+'/'+strcero(right(w_pede,4))
	show get w_pede
	if lastkey()=p_f4
		do fhelp with 'W_PEDE','','iwp004','f04','d4peri','d4peri','',(p_char),'w_retu'
   endif   
   if !seek(ptoi(w_pede),'f04')
   	if !w_show
	   	wait window 'Perํodo Invแlido'
	   endif
	   w_retu=.f.
	endif
endif

if w_vari='W_PEHA' or w_show
	w_peha=strcero(left(w_peha,2))+'/'+strcero(right(w_peha,4))
	show get w_peha
	if lastkey()=p_f4
		do fhelp with 'W_PEHA','','iwp004','f04','d4peri','d4peri','',(p_char),'w_retu'
   endif   
   if !seek(ptoi(w_peha),'f04')
   	if !w_show
	   	wait window 'Perํodo Invแlido'
	   endif
	   w_retu=.f.
	endif
endif

if w_vari='W_CLDE' or w_show
	w_clde=strcero(w_clde)
	show get w_clde
	if lastk()=p_f4
		do fhelp with 'W_CLDE','','iwp002','f02','d2clie','d2clie','',(p_strc),'w_retu'
	endif
endif

if w_vari='W_CLHA' or w_show
	w_clha=strcero(w_clha)
	show get w_clha
	if lastk()=p_f4
		do fhelp with 'W_CLHA','','iwp002','f02','d2clie','d2clie','',(p_strc),'w_retu'
	endif
	if w_clde>w_clha
		if !w_show
			wait wind 'Hasta debe ser mayor o igual que desde...'
		endif
		w_retu=.f.
	endif
endif
return(w_retu)
