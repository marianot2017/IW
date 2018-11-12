*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP004D.PRG -> Depurador de periodos
*)-H
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

*) Datos del proceso-----------------------------------------------------------

w_pfile='04d'
w_title='DEPURACION DE '+db_name('f04')

w_pede='/'
w_peha='/'

*) Apertura de archivos--------------------------------------------------------

fieldvec[1]='F00'
fieldvec[2]='F04'
fieldvec[3]='F05'
fieldvec[4]='F06'
fieldvec[5]='F07'
fieldvec[6]='F13'
fieldvec[7]='F15'

use (d0f00) in 0 alias 'f00'									&&(Parametros)
use (d0f04) in 0 alias 'f04' order 'd4peri' EXCLUSIVE &&(periodos)
use (d0f05) in 0 alias 'f05'                EXCLUSIVE &&(Comprobantes)
use (d0f06) in 0 alias 'f06' order 'd6inte' EXCLUSIVE &&(Arch.Alternativo)
use (d0f07) in 0 alias 'f07' order 'd7inte' EXCLUSIVE &&(Textos)
use (d0f13) in 0 alias 'f13'                EXCLUSIVE &&(Anticipos y retenciones)
use (d0f15) in 0 alias 'f15'                EXCLUSIVE &&(Mov.Complementarios)

if neterr()
	pop key
	pop menu _msysmenu
	fieldvec=''
	=closall()
	return
endif

fieldvec=''

*)-----------------------------------------------------------------------------

select f04
go top
w_pede=d4peri
go bott
w_peha=d4peri

do ssm998.mpr
do iws004d.spr

pop key
pop menu _msysmenu
=closall()
return

***vfp***
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons04d
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	on key label f9 do cons04d0
	keyboard '{f9}'
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons04d0
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
on key label f9
***vfp***

if !valid04d(.t.)
	wait wind 'Verifique los datos ingresados...'
	return
endif

if sino('Confirma Depuraci๓n ?.','N','Sistema')='N'
	return
endif

if begintran()

	sele f13
   wait wind nowait 'Procesando Anticipos y Rentenciones 13...'
   dele for between(ptoi(d13peri),ptoi(w_pede),ptoi(w_peha))
   
   sele f15
   wait wind nowait 'Procesando Movimientos Complementarios 15...'
   dele for between(ptoi(d15peri),ptoi(w_pede),ptoi(w_peha))
   
   sele f04
   wait wind nowait 'Procesando Perํodos 04...'
   dele for between(ptoi(d4peri),ptoi(w_pede),ptoi(w_peha))
   
   wait wind nowait 'Procesando Comprobantes 05 06 07...'
   sele f05
   go top
   do while !eof('f05')
      if between(ptoi(f05.d5peri),ptoi(w_pede),ptoi(w_peha))
      	if seek(f05->d5Inte,'f06')
      		=dele_rec('f06')
      	endif
      	if seek(f05->d5Inte,'f07')
      		=dele_rec('f07')
         endif
         =dele_rec('f05')
      endif
      skip in f05
	enddo
	
	*)rinde(04)
   *)rinde(05)
   *)rinde(06)
   *)rinde(07)
   *)rinde(13)
   *)rinde(15)
   
   =endtran()

endif
wait clear
=advsuave('La depuraci๓n de perํodos ha finalizado.','Informaci๓n')
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC valid04d
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
   if !seek(PtoI(w_pede),'f04')
   	if !w_show
	   	wait window 'Perํodo Inexistente'
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
   if !seek(PtoI(w_peha),'f04')
   	if !w_show
	   	wait window 'Perํodo Inexistente'
	   endif
	   w_retu=.f.
	endif	
	if ptoi(w_pede)>ptoi(w_peha)
		if !w_show
			wait wind 'Hasta debe ser mayor o igual que desde...'
		endif
		w_retu=.f.
	endif
endif
return(w_retu)
