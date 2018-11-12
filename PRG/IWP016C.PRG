*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP016C.PRG -> Copia de informes
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

*) Variables del proceso-------------------------------------------------------

w_pfile='16c'
w_title='COPIA DE INFORMES'

wIorig=space(3)
wIdest=space(3)

*) Apertura de archivos--------------------------------------------------------

fieldvec[1]='F16'
fieldvec[2]='F17'

use (d0f16) in 0 alias 'f16' order 'd16defi' &&(Def.Cabezera)
use (d0f17) in 0 alias 'f17' order 'd17defi' &&(Def.Renglones)

if neterr()
	pop key
	pop menu _msysmenu
	fieldvec=''
	=closall()
	return
endif

fieldvec=''

*)-----------------------------------------------------------------------------

do ssm998.mpr &&(menu Ayuda)
do iws016c.spr

pop key
pop menu _msysmenu
=closall()
return

***vfp***
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons16c
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	on key label f9 do cons16c0
	keyboard '{f9}'
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons16c0
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
on key label f9
***vfp***

if !valid16c(.t.)
	wait wind 'Verifique los datos ingresados...'
	return
endif
if sino('Confirma la Copia de informe ?.')='S'
	do copia16
endif
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC Copia16
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
wait wind nowait 'Procesando...'

=db_flock('f16')
=db_flock('f17')

*) Alta en f16

sele f16
=seek(wIorig,'f16')
scatter memvar
m.d16defi=wIdest
if begintran()
	=net_appe('f16')
	sele f16
	gather memvar
	=endtran()   
endif

*) Alta en f17

sele f17
***vfp***
*copy to (d0f99) for d17defi=wIorig
if version()='Visual FoxPro'
	copy to &d0f99 type fox2x for d17defi=wIorig
else
	copy to (d0f99) for d17defi=wIorig
endif
***vfp***
use (d0f99) in 0 alias 'f99' EXCLUSIVE

sele f99
repl all d17defi with wIdest
use

sele f17
appe from (d0f99)

unlock in f16
unlock in f17
dele file &d0f99..dbf

wait clear
=advsuave('Copia Concluํda. Acepte para continuar...','Informaci๓n')
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC valid16c
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_show

if empty(parameters())
	w_show=.f.
endif
w_retu=.t.
w_vari=varread()

wIorig=strcero(wIorig)
wIdest=strcero(wIdest)

if w_vari='WIORIG' or w_show
	do fhelp with 'WIORIG','','iwp016','f16','d16defi','d16defi','Informe origen inexistente...',(p_strc),'w_retu'
endif

if w_vari='WIDEST' or w_show
	if seek(wIdest,'f16') .or. seek(wIdest,'f17')
		if !w_show
			wait wind 'Informe destino existente...'
		endif
		w_retu=.f.
	endif
endif

if (wIorig=wIdest)
	if !w_show
		wait wind 'Origen y destino, deben ser diferentes...'
	endif
	w_retu=.f.
endif
return(w_retu)
