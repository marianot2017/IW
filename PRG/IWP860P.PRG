*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP860P.PRG -> Mantenimiento de Parametros de CITI
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

*) Variables internas----------------------------------------------------------

w_prog=upper(prompt())
w_alta=.f.
w_modicampo=.f.

*) Datos del proceso-----------------------------------------------------------

w_pfile='860p'
w_alias='F12'
w_title='A.F.I.P.  C.I.T.I.'

*) Apertura de archivos--------------------------------------------------------

fieldvec[1]='F12'
use (d0f12) in 0 alias 'f12' EXCLUSIVE

if neterr()
	pop key
	pop menu _msysmenu
	fieldvec=''
	=closall()
	return
endif

fieldvec=''

*)-----------------------------------------------------------------------------

sele f12
go top
scatter memvar

set skip of menu _msysmenu .t.
do ssm999.mpr

=skipbar('registro','all',.t.)
=skipbar('registro','agregar',.f.)
=skipbar('registro','Modificar',.f.)
=skipbar('registro','Calculadora',.f.)
=skipbar('registro','Salir',.f.)

on key label f1 do nada
do iws860p.spr

pop key
pop menu _msysmenu
=closall()
return

***vfp***
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc actu860p
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	on key label f9 do actu860p0
	keyboard '{f9}'
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc actu860p0
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
if !valid860p(.t.)
	wait wind 'Verifique los datos ingresados...'
	return
endif

if begintran()
	wait wind 'Actualizando parแmetros...' time .4
	sele f12
	gather memvar
	=endtran()
	w_modicampo=.f.

	clear read
endif
return

* ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc when860p
* ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
w_retu=.t.
w_vari=varread()
p_curobj=_curobj

*No va irclave() porque no hay clave

return w_retu

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC valid860p
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_show

if empty(parameters())
	w_show=.f.
endif

PRIVATE w_retu,w_vari
w_retu=.t.
w_vari=varread()
w_vread='m.'+w_vari

if w_vari='P0TONC' or w_show
	if !m.p0tonc$'NSR'
		if !w_show
			wait wind 'Opciones: [N,S,R]'
		endif
		w_retu=.f.
	endif
endif

if w_vari='P0TOND' or w_show
	if !m.p0tond$'NSR'
		if !w_show
			wait wind 'Opciones: [N,S,R]'
		endif
		w_retu=.f.
	endif
endif

if w_vari='P0DICI' or w_show
	w_dici=alltrim(m.p0dici)
	m.p0dici=subs(iif(subs(w_dici,len(w_dici),1)<>'\',w_dici+'\',w_dici)+spac(20),1,20)
endif

=ultdepri('P0DICI','m.p0tond')
=prideult('P0TOND','m.p0tond')

=modicampo()

return(w_retu)
