*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP900S.PRG -> Parametros del sistema
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

w_curobj=_curobj	  &&(Salva numero de objeto en el read activo)
w_alan=alias()		  &&(Salva alias vigente al llamado del programa)
w_wind=wontop()	  &&(Salva ventana vigente al llamado del programa)
w_rdlevel=rdlevel() &&(Salva Nro. de read activo)

*) Variables internas----------------------------------------------------------

w_prog=upper(prompt())
w_alta=.f.
w_agre=.not. mprot(p_user,w_prog,'Registro','Agregar')
w_modi=.not. mprot(p_user,w_prog,'Registro','Modificar')
w_recn=0
w_modicampo=.f.

*) Datos del archivo a mantener------------------------------------------------

w_pfile='900s'
w_alias='f12'
w_title=db_name(w_alias)

*) Apertura de archivos--------------------------------------------------------

fieldvec[1]='F12'

use (d0f12) in 0 alias 'f12' EXCLUSIVE &&(Parametros del sistema)

if neterr()
	pop key
	pop menu _msysmenu
	fieldvec=''
	if .not. empty(w_wind)
		acti wind (w_wind)
	endif
	=closall()
	return
endif

fieldvec=''

*)-----------------------------------------------------------------------------

select (w_alias)
go top
scatter memvar

set skip of menu _msysmenu .t.
do ssm999.mpr	&&(menu registro)

=skipbar('registro','all',.t.)
=skipbar('registro','Agregar'   ,.f.)
=skipbar('registro','Modificar',.f.)
=skipbar('registro','Calculadora',.f.)
=skipbar('registro','Salir',.f.)

on key label f1 do nada
if w_agre
	on key label f9 keyboard '{ctrl+f9}'
endif

do iws900s.spr

pop key
pop menu _msysmenu
=closall()

return

***vfp***
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc actu900s
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	on key label f9 do actu900s0
	keyboard '{f9}'
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc actu900s0
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
if !valid900s(.t.)
	wait wind 'Verifique los datos ingresados...'
	return
endif

=db_capt(w_alias)

if st_rlock(w_alias)
	if begintran()
		wait wind 'Actualizando parแmetros...' time .4

		sele (w_alias)
		go top
		gather memvar
		d0peta=m.p0peta
		d0peto=p0peto
		d0paco=trim(m.p0paco)
		d0deci=m.p0deci
		d0deno=m.p0deno
		d0disc=m.p0disc
		d0diga=p0diga

		=inico()
		wdespl=.t.
		w_modicampo=.f.

		if at('CG',upper(d0paco))>0 	&& blanquea dispostivo de iva dos en conta win
			fieldvec[1]='F12x'
			d0f12x=d0paco+'cof012'
			use &d0f12x in 0 alias f12x
			if !neterr()
				sele f12x
				if !empty(p0disi)
					=db_rlock('f12x')
					replace p0disi with '  '
					unlock in f12x
				endif
				use
			endif
			sele (w_alias)
		endif

		=endtran()

	endif

endif

clear read

return

* ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc when900s
* ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
w_retu=.t.
w_vari=varread()
p_curobj=_curobj

*No va irclave() porque no hay clave

if m.p0toco='S'
	show get m.p0disc enable
else
	show get m.p0disc disable
endif

return w_retu

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC valid900s
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
paramet w_show
if parameters()=0
	w_show=.f.
endif

w_retu=.t.
w_vari=varread()
w_vread='m.'+w_vari


if w_vari='P0TOCO' or w_show
	if m.p0toco='S'
		show get m.p0disc enable
	else
		show get m.p0disc disable
	endif
	if !(m.p0toco ='S' or m.p0toco='N')
		wait wind 'Debe Ingresar S o N'
		w_retu=.f.
	endif

endif

if w_vari='P0DISC' or w_show
	if !empty(m.p0disc) and m.p0toco='S'
		if !mfile(m.p0disc+'\IV\ivf010.dbf')
			wait wind 'Directorio de IVA no existe en el dispositivo indicado'
			m.p0disc=''
			w_retu=.f.
		endif
	endif

endif

if w_vari='P0PACO' or w_show
	if ! empty(m.p0paco)

		wcheq='/\:0123456789'
		wwpaco=''

		for i=1 to len(trim(m.p0paco))
			wl=subs(m.p0paco,i,1)
			if .not. (isalpha(wl) .or. at(wl,wcheq)<>0)
				w_retu=.f.
				exit
			endif
			wl=iif(at(wl,wcheq)=1,'\',wl)
			wwpaco=wwpaco+wl
		next

		if !w_retu
			wait wind 'Error de Sintaxis en el Directorio...'
		else
*			m.p0paco=subs(iif(subs(m.p0paco,len(wwpaco),1)<>'\',wwpaco+'\',wwpaco)+spac(15),1,20)	&& R.01 Mariano
			m.p0paco=subs(iif(subs(m.p0paco,len(wwpaco),1)<>'\',wwpaco+'\',wwpaco)+spac(20),1,20)	&& R.01 Mariano
			if !file(alltrim(m.p0paco)+'cof010.cdx')
				wait wind 'Directorio Err๓neo...'
				w_retu=.f.
			endif
		endif
	endif
endif

if w_vari='P0DIGA' or w_show
	if !empty(m.p0diga)
		if !mfile(m.p0diga+'\GA\GAF001.DBF')
			wait wind 'Directorio de GENERADOR DE DATOS no existe en el dispositivo indicado'
			m.p0diga=''
			w_retu=.f.
		endif
	endif
endif

=ultdepri('P0PACO','m.p0diga')
=prideult('P0DIGA','m.p0diga')

=modicampo()

return(w_retu)
