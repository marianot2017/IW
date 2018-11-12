*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP900F.PRG -> Parแmetros Copiativo Subdiario IVA
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
w_pfile='900f'
w_title='PARAMETROS COPIATIVO SUBDIARIO DE IVA'

*) Apertura de archivos--------------------------------------------------------

fieldvec[1]='F00'
use (d0f00) in 0 alias 'f00' &&(Parametros)

if neterr()
	pop key
	pop menu _msysmenu
	fieldvec=''
	=closall()
	return
endif

fieldvec=''

*)-----------------------------------------------------------------------------
SELE f00
SCATTER MEMVAR

do ssm998.mpr

=skipbar('registro','elegir',.t.)

do iws900f.spr

pop key
pop menu _msysmenu

=closall()

dele file &d0f99.c.dbf

return

*)ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC when900f
*)ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
w_retu=.t.
w_vari=varread()
p_curobj=_curobj

return(w_retu)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC valid900f
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_show

w_retu=.t.
w_vari=varread()
w_vread='m.'+w_vari

return(w_retu)

***vfp***
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons900f
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	on key label f9 do cons900f0
	keyboard '{f9}'
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons900f0
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
on key label f9
***vfp***

if !valid900f(.t.)
	wait wind 'Verifique los datos ingresados...'
	return
endif

if sino('Confirma ?.','N','Parแmetros Subdiario de IVA')='N'
	return
endif

if !valid900f(.t.)
	wait wind 'Verifique los datos ingresados...'
	return
endif

if begintran()

	wait wind 'Actualizando parแmetros...' time .4

	gather memvar

	=endtran()

	clear read

endif
return
