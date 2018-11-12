*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP863.PRG -> Reasignacion de Tipo de Operacion
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

w_pfile='863'
w_title=upper(prompt())

w_veco=1
w5tipo='V'
wtiop ='EOXY'

w_comp=1
w5tico=''

wdperi='/'
whperi='/'
w5tifo=''
w_tiop=''

*) Apertura de archivos--------------------------------------------------------

fieldvec[1]='F00'
fieldvec[2]='F04'
fieldvec[3]='F05'

use (d0f00) in 0 alias 'f00'								&&(Parametros)
use (d0f04) in 0 alias 'f04' order 'd4peri' EXCLU &&(periodos)
use (d0f05) in 0 alias 'f05' 					  EXCLU &&(comprobantes)

if neterr()
	pop key
	pop menu _msysmenu
	fieldvec=''
	=closall()
	return
endif

fieldvec=''

*)-----------------------------------------------------------------------------

go bott in f04
whperi=f04->d4Peri
go top in f04
wdperi=f04->d4Peri
w5tifo=spac(1)
w_tiop=spac(1)

do ssm998.mpr
do iws863.spr

pop key
pop menu _msysmenu
=closall()
return

***vfp***
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons863
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	on key label f9 do cons863x
	keyboard '{f9}'
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons863x
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
on key label f9
***vfp***

if !valid863(.t.)
	wait wind 'Verifique los datos ingresados...'
	return
endif

if sino('Confirma Reasignaci๓n de tipo de operaci๓n ?.')='S'
	if begintran()
		wait wind nowait 'Procesando...'
      sele f05
      repl all d5Tiop with w_tiop;
      	for d5tipo$w5tipo and;
             d5tico$w5tico and;
             d5tifo$w5tifo and;
             between(PtoI(d5peri),PtoI(wdperi),PtoI(whperi))		
		=endtran()		
		wait clear
		=advsuave('Reasignaci๓n finalizada. Acepte para continuar...','Informaci๓n')
	endif
endif
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC valid863
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_show

if empty(parameters())
	w_show=.f.
endif

PRIVATE w_retu,w_vari
w_retu=.t.
w_vari=varread()

if w_vari='W_VECO' or w_show
	if w_veco=1
		w5tipo='V'
		wtiop ='EOXY'
	else
		w5tipo='C'
		wtiop ='EOI'
	endif
endif

if w_vari='W_COMP' or w_show
	do case
	case w_comp=1
		w5tico='A'
	case w_comp=2
		w5tico='C'
	case w_comp=3
		w5tico='B'
	case w_comp=4
		w5tico='?'
	case w_comp=5
		w5tico='@'
	endcase
endif

if w_vari='W5TIFO' or w_show
	if empty(w5tifo)
		if !w_show
			wait wind 'Debe especificar el tipo de formulario.'
		endif
		w_retu=.f.
	endif
endif

if w_vari='W_TIOP' or w_show
	if !w_tiop$wtiop
		if !w_show
			wait wind 'Opciones permitidas: '+wtiop
		endif
		w_retu=.f.
	endif
endif

if w_vari='WDPERI' or w_show
	wdperi=strcero(left(wdperi,2))+'/'+strcero(right(wdperi,4))
	show get wdperi
	if lastkey()=p_f4
		do fhelp with 'WDPERI','','iwp004','f04','d4peri','d4peri','',(p_char),'w_retu'
   endif   
   if !seek(PtoI(wdperi),'f04')
   	if !w_show
	   	wait window 'Perํodo Inexistente'
	   endif
	   w_retu=.f.
	endif
endif

if w_vari='WHPERI' or w_show
	whperi=strcero(left(whperi,2))+'/'+strcero(right(whperi,4))
	show get whperi
	if lastkey()=p_f4
		do fhelp with 'WHPERI','','iwp004','f04','d4peri','d4peri','',(p_char),'w_retu'
   endif
   if !seek(ptoi(whperi),'f04')
   	if !w_show
	   	wait window 'Perํodo Inexistente'
	   endif
	   w_retu=.f.
	endif
endif

if ptoi(wdperi)>ptoi(whperi)
	if !w_show
		wait wind 'Hasta debe ser mayor o igual que desde...'
	endif
	w_retu=.f.
endif
return(w_retu)
