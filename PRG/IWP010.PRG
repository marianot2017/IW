*-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*-H Programa        : iwp010.prg -> Mantenimiento de Empresas
*-H
*-H Release			: 05
*-H Fecha de inicio :
*-H Fecha de fin    :
*-H
*-H Actualizaciones
*-H
*-H  Fecha       Programmer      Comentarios
*-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
parameters w_pref,w_call
private all like w_*

if parameters()=0
	w_call=.f.
	w_pref=''
endif

w_prog=upper(prompt())

*** Salvado de estados anteriores

push key clear      && Salva estados de teclas
push menu _msysmenu && Salva estado de menu

w_curobj = _curobj     && Salva numero de objeto en el read activo
w_alan   = alias()       && Salva alias vigente al llamado del programa
w_wind   = wontop()      && Salva ventana vigente al llamado del programa
w_rdlevel= rdlevel()  && Salva Nro. de read activo

** Variables internas

w_prog = program()
w_alta = .f.
w_agre = .not. mprot(p_user,w_prog,'Registro','Agregar')
w_modi = .not. mprot(p_user,w_prog,'Registro','Modificar')
w_recn = 0
w_modicampo=.f.
w_empalt=.f.
** Datos del archivo a mantener
w_pfile='10'
w_clave='m.P0COEM'
w_alias='F'+w_pfile
w_title=db_name(w_alias)

wxcoem=''
wsour=''

if .not. w_call

	fieldvec[1] = 'F10'
	fieldvec[2] = 'F66'

	use &d0f10 in 0 alias f10 order p0coem          && Empresas
	use &d0f66 in 0 alias f66 order d66user         && Usuarios por Empresas

	if neterr()

		pop key
		pop menu _msysmenu

		if .not. empty(w_wind)
			acti wind (w_wind)
		endif

		=closall()
		return

	endif

	fieldvec=''

	sele f10
	copy struc to &d0f99 with cdx
	copy to &d0f99.a with cdx

	use &d0f99.a in 0 alias f99a orde p0coem exclu
	use &d0f99   in 0 alias f99  orde p0coem exclu

	go top in f66

	do while !eof('f66')
		if f66.d66user=ps_user
			if seek(f66.d66empr,'f10')
				sele f10
				scatter memvar
				sele f99
				appe blan
				gather memvar
			else
				wait 'La empresa '+f66.d66empr+' asignada al usuario '+ps_user+;
					' fue eliminada !! ' wind
			endif
		endif
		skip in f66
	enddo

	sele f66
	use

	sele f10
	* Para que vea nada mas que las empresas que tiene asignadas
	set rela to p0coem into f99
	set filt to !empty(f99.p0coem)
	go top

endif

sele &w_alias
if .not. w_call
	=db_goeof(w_alias)
endif
scatte memvar blank

set skip of menu _msysmenu .t.

do ssm999.mpr && Menu General
=skipbar('Registro','Insertar',.t.)

on key label f1 do nada
if w_agre
	on key label f9 keyb '{ctrl+f9}'
endif

=skipbar('Registro','Insertar',.t.)

if w_call
    ***vfp***
*	do brow10
    if version()='Visual FoxPro'
		*** cambio el lastkey() ***
		keyboard '{tab}'
		=inkey()
		DO wbrow WITH 'brow10()'
    else
   		do brow10
    endif
    ***vfp***
	if w_alta
		w_alta=.f.
		scatter memvar blank

		=skipbar('Registro','Eliminar'  ,.t.)
		=skipbar('Registro','Modificar' ,.t.)

		do iws010.spr

	endif
else
	if !empty(w_wind)
		deac wind (w_wind)
	endif

	***vfp***
    if version()='Visual FoxPro'
	    on key label esc do actsalir
	endif
	***vfp***
	do iws010.spr
    if version()='Visual FoxPro'
	    on key label esc
	endif
	***vfp***

endif

pop key
do ssm995.mpr	&& R.05 Mariano (puesto porque en vfp quedan activas los key del menu usado y de esta manera se limpian)
pop menu _msysmenu


if w_call
	set orde to 1 in (w_alias)
	if .not. empty(w_alan)
		sele &w_alan
	endif
	if .not. empty(w_wind)
		acti wind (w_wind)
	endif

else
	=closall()
endif

return

*-R อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc alta10
*-R อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

if w_call
	w_alta=.t.
	deac wind (w_title)
endif

return

*-R อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc baja10
*-R อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

do clav10

if eof(w_alias) .or. .not. st_rlock(w_alias)
	return
endif

if m.p0coem=='00'
	wait 'C๓digo restringido por el sistema' wind nowait
	return
endif

if m.p0coem=d0coem
	wait 'Empresa activa actualmente' wind nowait
	return
endif

wait 'Serแn borrados todos los datos de esta empresa' wind nowait

if sino('Acepta eliminaci๓n ? ')='S'

	if !lockemp('')
		=errornet(075)
		return
	endif

	sele f77
	use
	do while .t.

		fieldvec[1] = 'F77'

		use &d0f77 in 0 alias f77 excl

		if .not. neterr()
			exit
		endif
		=errornet(077)
	enddo

	fieldvec=''
	
	if deledir(trim(m.p0path))

		=dele_rec('f10')
		* Exclusive para packear
		* ----------------------
		sele f10
		use
		do while .t.

			fieldvec[1] = 'F10'

			use &d0f10 in 0 alias f10 excl

			if .not. neterr()
				exit
			endif
		enddo

		fieldvec=''

		sele f10
		pack
		use
		* Shared
		* ------
		do while .t.

			fieldvec[1] = 'F10'

			use &d0f10 in 0 alias f10 order p0coem

			if .not. neterr()
				exit
			endif
		enddo

		fieldvec=''


		do while .t.

			fieldvec[1] = 'f66'

			use &d0f66 in 0 alia f66 excl

			if .not. neterr()
				exit
			endif
		enddo

		fieldvec=''

		sele f66
		dele all for d66empr=m.p0coem
		pack

		use

	endif

	sele f77
	dele all for d77empr=m.p0coem
	pack

	use

	nf_reem=0	&& Fuerza actualizacion de puntero registro terminal-empresa
	=login('S')
	=login('E',.t.)

	sele f10

	=audi('BAE S','','',m.p0coem)

	=unlockemp('')

	nf_rean=nf_reem		&& Fuerza actual.registro empresa anterior con la que estaba
	
	unlock in (w_alias)
	do siguiente
endif

return

*-R อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc clav10
*-R อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

w_orde=order(w_alias)

&w_clave=strcero(&w_clave)
show get &w_clave

if w_recn<>0
	w_recn=recno(w_alias)
endif

set orde to 1 in (w_alias)

if seek(w_pref+&w_clave,w_alias)
	* Modificacion

	if w_recn<>recno(w_alias) .or. !st_rlock(w_alias)
		do work10
		w_recn=recno(w_alias)
	endif

	* Modificacion
	show get m.p0vein enable
	show get m.p0vees enable
	show get m.p0veda enable

	w_empalt=.f.

	if w_call
		unlock in (w_alias)
		clear read
		return
	endif
else
	* Alta


	if seek(w_pref+&w_clave,'F99A')
		wait wind 'C๓digo Reservado' time .4
		show gets
		return .f.
	endif

	w_cclav=&w_clave
	if w_recn<>recno(w_alias)
		do work10
		w_recn=recno(w_alias)
	else
		if lastkey()<>p_f9
			w_spac=len(m.p0path)
			m.p0path='\iw\'+m.p0coem+'\'+space(w_spac-len('\iw\'+m.p0coem+'\'))
			m.p0vein=wversexe
			m.p0vees=wversexe
			m.p0veda=wversexe
		endif
	endif

	&w_clave=w_cclav

	w_empalt=.t.
	* Alta
	show get m.p0vein disable
	show get m.p0vees disable
	show get m.p0veda disable
*===>
	SHOW GET m.p0path
*===>
endif

set orde to w_orde in (w_alias)

return .t.
* อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc when10
* อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
w_retu=.t.
w_vari=varread()
p_curobj=_curobj

if w_vari='P0BACK'
	if !w_empalt and sino('Actualiza el destino?', 'No', 'DESTINO DE RESGUARDO DE EMPRESA')='S'
	  w_dire=alltr(right(m.p0back,48))
	  wDrF=ElijeDisp('DESTINO DE RESGUARDO DE EMPRESA',alltr(m.p0back),.t.,.t.)
	  m.p0back=lower(wDrF+w_dire)
	  show get m.p0back
	  keyboard '{Enter}'
	else
		w_retu=.f.
	endif
endif

=irclave('P0COEM','m.p0empr')

return w_retu

* อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc valid10
* อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
parameter w_show

***vfp***
if version()='Visual Fox' and (w_pfile!='10' or (!wexist('IWS010') and wontop()!='IWS010')) and w_show
	if lastkey()=p_esc
		return .t.
	else
		return .f.
	endif
endif
***vfp***

if parameters()=0
	w_show=.f.
endif

w_retu=.t.
w_vari=varread()
w_vread='m.'+w_vari

*===> Permitํa empresa sin nombre
IF w_vari = "P0EMPR"	.OR. w_show
	IF EMPTY(m.p0empr)
		IF !w_show
			WAIT WIND "DEBE INGRESAR EL NOMBRE DE LA EMPRESA"
		ENDIF
		IF w_retu
				_curobj	= 2
				w_retu = .F.
		ENDIF
	ENDIF
ENDIF			
*===>

if w_vari='P0VEIN' .or. w_show
*if w_vari='P0VEIN' 
	if !between(val(m.p0vein),val(wversexe)-1,val(wversexe))
		if !w_show
			wait 'Debe ser ['+strcero(str(val(wversexe)-1,3))+','+wversexe+']' wind
		endif
		w_retu=.f.
	endif
endif

if w_vari='P0VEES' .or. w_show
*if w_vari='P0VEES'
	if !between(val(m.p0vees),val(wversexe)-1,val(wversexe))
		if !w_show
			wait 'Debe ser ['+strcero(str(val(wversexe)-1,3))+','+wversexe+']' wind
		endif
		w_retu=.f.
	endif
endif

if w_vari='P0VEDA' .or. w_show
*if w_vari='P0VEDA'
	if !between(val(m.p0veda),val(wversexe)-1,val(wversexe))
		if !w_show
			wait 'Debe ser ['+strcero(str(val(wversexe)-1,3))+','+wversexe+']' wind
		endif
		w_retu=.f.
	endif
endif


*===> Agregado del IF para que si hay errores quede posicionado en el primer campo con error
IF w_retu
		if w_empalt
				=ultdepri('P0COEM','m.p0empr')
				=prideult('P0EMPR','m.p0empr')
		else
			=ultdepri('P0COEM','m.p0veda')
			=prideult('P0VEDA','m.p0veda')
		endif
ENDIF
*===>	

=modicampo('P0COEM')

sele f10
return w_retu

***vfp***
** no modificar por VFP **
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc actu10
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
***vfp***

if !w_agre .and. !w_modi
	return
endif

DO clav10				&& R.01 Ra๚l   por el tema de agregar el control de versi๓n - 1 

If !Valid10(.t.)
    wait wind 'Debe completar todos los datos' time 1
    Return(.f.)
Endif

do clav10

if .not. eof(w_alias) .and. .not. st_rlock(w_alias)
	return
endif

=db_capt(w_alias)

w_orde=order(w_alias)
set orde to 1 in (w_alias)

if .not. seek(w_pref+&w_clave,w_alias)

	if w_agre
		if hayspace()
			if sino('Toma Archivos de otra Empresa ?','No')='S'
				w_toma='S'
				do empr10
				if lastkey()=p_esc
					return
				endif

				if !lockemp(wxcoem)
					=errornet(051)
					return
				endif

			else
				if !lockemp('00')	&& toma archivos vacios de la empresa 00 generica
					=errornet(051)
					return
				endif

				w_toma='N'
				wxcoem=''

				show gets

			endif

			m.p0vein=wversexe
			m.p0vees=wversexe
			m.p0veda=wversexe

			show gets

			if sino('Confirma alta ?','No')='S'
				if w0mop$'D' and reccount('f10')>=2
					=advsuave('Versi๓n Reducida. S๓lo Dos Empresas')
					return
				endif

				if seek(m.p0coem,'f10')
					wait 'C๓digo Reservado' wind nowait
					return
				endif

				if begintran()

*===> Verificar que no exista el directorio y copiar archivos
					IF copy10()
*===>
							=db_flock('f10')
		
							wait ' Agregando registro... ' wind time .4
	
							=audi('ALE S','','',m.p0coem)

							if !seek(m.p0coem,'f10')
									=net_appe('f10')
									m.p0nive='6'
			
									gather memv
									w_modicampo=.f.
									=db_rlock(w_alias)
									if .not. w_call
											_curobj=1
									endif
*===> Tiene que ir mแs arriba y verificar que vuelva con .T.
*						do copy10
*===>
									do makeuser
							else
								wait 'Dado de alta por otro Usuario' wind nowait
							endif
*===>
					ENDIF
*===>
SELE 0
xf00 = ALLT(p0path) + 'ivf000'
USE &xf00
REPLA p0emp			WITH f10.p0empr
USE
					unlock in f10

					=endtran()

				endif

			endif

			if .not. empty(wxcoem)
				=unlockemp(wxcoem)
			else
				=unlockemp('00')
			endif

			wait clear

			=login('E')

		endif

	endif

	select (w_alias)
else
	if st_rlock(w_alias)
		if w_modi

			if begintran()

				wait ' Modificando registro... ' wind time .4
				gather memvar
				w_modicampo=.f.
				=audi('ALE S','','',m.p0coem)

				=endtran()

			endif

		endif
	else
		do work10
	endif
endif

unlock in f98

set order to (w_orde) in (w_alias)
if w_call
	unlock in (w_alias)
	clear read
endif

SELE (w_alias)

return

*-R อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc work10
*-R อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

w_clav=m.p0coem
scatter memvar
if eof(w_alias)
	store .t. to m.d1tal1,m.d1tal2,m.d1tal3,m.d1tal4,m.d1tal5,m.d1tal6,m.d1tal7

	w_spac=len(m.p0path)
	m.p0path='\iw\'+w_clav+'\'+space(w_spac-len('\iw\'+w_clav+'\'))
	show get m.p0path

endif

if w_rdlevel<>rdlevel() and iif(version()='Visual Fox', !w_call,.t.)	&& ***vfp***
	show gets
	=valid10(.t.)
endif

if .not. eof(w_alias)
	=rlock(w_alias)
	if .not. st_rlock(w_alias,.t.)
		_curobj=1
	endif
endif

return .t.

*-R อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc brow10
*-R อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

activate screen

w_orde=order(w_alias)

push menu _msysmenu


* El alta es unicamente a traves de la pantalla principal
=skipbar('Registro','Insertar' ,.t.)

=skipbar('Registro','Eliminar' ,.t.)
=skipbar('Registro','Consultar',.t.)
=skipbar('Registro','Agregar'  ,.t.)
=skipbar('Registro','Elegir'   ,.t.)
=skipbar('Registro','Modificar',.t.)

set orde to 2 in (w_alias)

if eof(w_alias)
	go top in (w_alias)
endif

defi wind w_most from 0,0 to 15,79 color scheme 10

browse  ;
	fields p0coem :h='C๓digo':v=fenter() :f ,;
	p0empr :h='Nombre' ,;
	p0path :h='Path'    ;
	title w_title ;
	nomenu ;
	noedit ;
	nodelete ;
	valid :f salebrow(lastkey()) ;
	font 'Arial', 10 ;
	noappend ;
	window w_most


rele wind w_most

set orde to w_orde in (w_alias)

if !w_call
	do work10
endif

pop menu _msysmenu

***vfp***
IF version()='Visual FoxPro'
	if w_call
		rele wind wbrow
	else
		clear read
		on key label esc do actsalir
	endif
endif
***vfp***

return
*-R อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc busc10
*-R อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

w_orde=order(w_alias)
*set orde to 2 in (w_alias)
do busca
if .not. eof(w_alias)
	do work10
endif
set orde to w_orde in (w_alias)
return

*-R อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc orde10
*-R อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

do case
	case upper(prompt())=upper('Ordenado Por C๓digo')
		set orde to 1 in (w_alias)
	case upper(prompt())=upper('Ordenado Por Descripci๓n')
		set orde to 2 in (w_alias)
endcase

if wexist(w_title)
	show wind (w_title) refresh
endif

return

*-P ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
function empr10
*-P ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

if eof('f10')
	w_re10=0
else
	w_re10=recno('f10')
endif

go top in f10
w_empr=1
do while .t.

	do iws0102.spr
	if lastkey()=p_esc
		return
	endif
	
	if lastkey()=p_enter
		exit
	endif

enddo

m.p0vein=f10->p0vein
m.p0vees=f10->p0vees
m.p0veda=f10->p0veda
wxcoem=f10->p0coem

wsour=alltrim(f10->p0path)+'ivf0'

if .not. empty(w_re10)
	go w_re10 in f10
else
	=db_goeof('f10')
endif

return

*-----------------------------------------------------------------------------
procedure copy10   && creacion de subdirectorio y copia efectiva de archivos
*-----------------------------------------------------------------------------
*set cursor off
wait 'Creando nueva empresa' wind nowait

wmacro=subs(trim(m.p0path),1,len(trim(m.p0path))-1)

if !creadir(wmacro)=0	&& no creo el directorio
*===>Mensaje en ventana, posicionamiento del cursor en el c๓digo y devuelve .F.
	wait WIND "No se pudo crear directorio de la Empresa: "+m.p0empr
	_curobj = 1
	return .F.
*===>
endif

wdest=trim(m.p0path)+'ivf0'

if w_toma='S'	&& toma datos de otra empresa

	wmacro1=wsour+'01.dbf'  && cuentas
	wmacro2=wdest+'01.dbf'
	n0f01=wsour+'01'

	* Se intenta Bloquear el Archivo
	* ------------------------------
	do while .t.

		fieldvec[1] = 'F01'

		use &n0f01 in 0 alias f01 excl

		if .not. neterr()
			exit
		endif

		wait "Inhabilite terminal que usa Empresa: "+wxcoem

	enddo
	sele f01
	&& R.03b Mariano
*	copy to &wmacro2
	***vfp***
	IF version()='Visual FoxPro'
		copy to &wmacro2 type fox2x   && Parametros
	else
		copy to &wmacro2   && Parametros
	endif
	&& R.03e Mariano
	* Cierre
	* ------
	use

	wmacro1=wsour+'08.dbf'  && Facturas tipo
	wmacro2=wdest+'08.dbf'
	n0f08=wsour+'08'
	* Se intenta Bloquear el Archivo
	* ------------------------------
	do while .t.

		fieldvec[1] = 'F08'

		use &n0f08 in 0 alias f08 excl

		if .not. neterr()
			exit
		endif

		wait "Inhabilite terminal que usa Empresa: "+wxcoem wind nowait
	enddo
	sele f08
	&& R.03b Mariano
*	copy to &wmacro2
	***vfp***
	IF version()='Visual FoxPro'
		copy to &wmacro2 type fox2x   && Parametros
	else
		copy to &wmacro2   && Parametros
	endif
	&& R.03e Mariano

	* Cierre
	* ------
	use

else	&& Crea empresa vacia

	go top

	wsour=trim(f10->p0path)+'ivf0'

	wmacro=wdest+'01'
	n0f01=wsour+'01'
	do while .t.

		fieldvec[1] = 'F01'

		use &n0f01 in 0 alias f01 excl
		if .not. neterr()
			exit
		endif

		wait "Inhabilite terminal que usa Empresa: "+wxcoem wind nowait
	enddo
	sele f01
	&& R.03b Mariano
*	copy stru to &wmacro
	copy stru to &d0f99.y
	sele 0
	use &d0f99.y in 0 alias 'f99y'
	***vfp***
	IF version()='Visual FoxPro'
		copy to &wmacro type fox2x
	else
		copy to &wmacro
	endif
	***vfp***
	use in f99y
	erase &d0f99.y.dbf
	sele f01
	&& R.03e Mariano
	
&& R.02b Ra๚l Crea Jurisdicci๓n Capital = C xa q no salten los eventuales
	USE &wmacro	IN 0 ALIAS f01b
	SELE f01b
	APPE BLANK
	REPLA d1tip WITH 'JU', d1cla WITH 'C', d1des WITH 'Capital Federal', d1por1 WITH 3, d1tdgi WITH '00'
	USE
	SELE f01	
&& R.02e Ra๚l
	use

	wmacro=wdest+'08'
	n0f08=wsour+'08'
	do while .t.

		fieldvec[1] = 'F08'

		use &n0f08 in 0 alias f08 excl
		if .not. neterr()
			exit
		endif

		wait "Inhabilite terminal que usa Empresa: "+wxcoem wind nowait
	enddo
	sele f08
	&& R.03b Mariano
*	copy stru to &wmacro
	copy stru to &d0f99.y
	sele 0
	use &d0f99.y in 0 alias 'f99y'
	***vfp***
	IF version()='Visual FoxPro'
		copy to &wmacro type fox2x
	else
		copy to &wmacro
	endif
	***vfp***
	use in f99y
	erase &d0f99.y.dbf
	sele f08
	&& R.03e Mariano
	use

endif

wmacro1=wsour+'00.dbf'  && parametros
wmacro2=wdest+'00.dbf'
n0f00=wsour+'00'
* Se intenta Bloquear el Archivo
* ------------------------------
do while .t.

	fieldvec[1] = 'F00'

	use &n0f00 in 0 alias f00 excl
	if .not. neterr()
		exit
	endif

	wait "Inhabilite terminal que usa Empresa: "+wxcoem wind nowait
enddo
sele f00
&& R.03b Mariano
*copy to &wmacro2
***vfp***
IF version()='Visual FoxPro'
	copy to &wmacro2 type fox2x   && Parametros
else
	copy to &wmacro2   && Parametros
endif
&& R.03e Mariano

* Cierre
* ------
use

wmacro=wdest+'02'		&& Clientes
n0f02=wsour+'02'
do while .t.

	fieldvec[1] = 'F02'
	
	use &n0f02 in 0 alias f02 excl
	if .not. neterr()
		exit
	endif

	wait "Inhabilite terminal que usa Empresa: "+wxcoem wind nowait
enddo
sele f02
copy for d2clie>='99990' to &wmacro   && Copia los Clientes Eventuales
use

wmacro=wdest+'03'		&& Proveedores
n0f03=wsour+'03'
do while .t.

	fieldvec[1] = 'F03'

	use &n0f03 in 0 alias f03 excl
	if .not. neterr()
		exit
	endif

	wait "Inhabilite terminal que usa Empresa: "+wxcoem wind nowait
enddo
sele f03
copy for d3prov>='99990' to &wmacro   && Copia los Proveedores Eventuales
use

wmacro=wdest+'04.dbf'	 && Periodos
n0f04=wsour+'04'
* Se intenta Bloquear el Archivo
* ------------------------------
do while .t.

	fieldvec[1] = 'F04'

	use &n0f04 in 0 alias f04 excl
	if .not. neterr()
		exit
	endif

	wait "Inhabilite terminal que usa Empresa: "+wxcoem wind nowait
enddo
sele f04
&& R.03b Mariano
*copy stru to &wmacro
copy stru to &d0f99.y
sele 0
use &d0f99.y in 0 alias 'f99y'
***vfp***
IF version()='Visual FoxPro'
	copy to &wmacro type fox2x
else
	copy to &wmacro
endif
***vfp***
use in f99y
erase &d0f99.y.dbf
sele f04
&& R.03e Mariano
use

wmacro=wdest+'05'		&& Comprobantes
n0f05=wsour+'05'
do while .t.

	fieldvec[1] = 'F05'

	use &n0f05 in 0 alias f05 excl
	if .not. neterr()
		exit
	endif

	wait "Inhabilite terminal que usa Empresa: "+wxcoem wind nowait
enddo
sele f05
&& R.03b Mariano
*copy stru to &wmacro
copy stru to &d0f99.y
sele 0
use &d0f99.y in 0 alias 'f99y'
***vfp***
IF version()='Visual FoxPro'
	copy to &wmacro type fox2x
else
	copy to &wmacro
endif
***vfp***
use in f99y
erase &d0f99.y.dbf
sele f05
&& R.03e Mariano
use

wmacro=wdest+'06'		&& Nombres Alternativos de Facturas
n0f06=wsour+'06'
do while .t.

	fieldvec[1] = 'F06'

	use &n0f06 in 0 alias f06 excl
	if .not. neterr()
		exit
	endif

	wait "Inhabilite terminal que usa Empresa: "+wxcoem wind nowait
enddo
sele f06
&& R.03b Mariano
*copy stru to &wmacro
copy stru to &d0f99.y
sele 0
use &d0f99.y in 0 alias 'f99y'
***vfp***
IF version()='Visual FoxPro'
	copy to &wmacro type fox2x
else
	copy to &wmacro
endif
***vfp***
use in f99y
erase &d0f99.y.dbf
sele f06
&& R.03e Mariano
use

wmacro=wdest+'07'		&& Textos Alternativos de Facturas
n0f07=wsour+'07'
do while .t.

	fieldvec[1] = 'F07'

	use &n0f07 in 0 alias f07 excl
	if .not. neterr()
		exit
	endif

	wait "Inhabilite terminal que usa Empresa: "+wxcoem wind nowait
enddo
sele f07
&& R.03b Mariano
*copy stru to &wmacro
copy stru to &d0f99.y
sele 0
use &d0f99.y in 0 alias 'f99y'
***vfp***
IF version()='Visual FoxPro'
	copy to &wmacro type fox2x
else
	copy to &wmacro
endif
***vfp***
use in f99y
erase &d0f99.y.dbf
sele f07
&& R.03e Mariano
use

wmacro=wdest+'09'       && Cliente - Facturas Tipo
n0f09=wsour+'09'
do while .t.

	fieldvec[1] = 'F09'

	use &n0f09 in 0 alias f09 excl
	if .not. neterr()
		exit
	endif

	wait "Inhabilite terminal que usa Empresa: "+wxcoem wind nowait
enddo
sele f09
&& R.03b Mariano
*copy stru to &wmacro
copy stru to &d0f99.y
sele 0
use &d0f99.y in 0 alias 'f99y'
***vfp***
IF version()='Visual FoxPro'
	copy to &wmacro type fox2x
else
	copy to &wmacro
endif
***vfp***
use in f99y
erase &d0f99.y.dbf
sele f09
&& R.03e Mariano
use

wmacro=wdest+'13'       && Ingresos Brutos
n0f13=wsour+'13'
do while .t.

	fieldvec[1] = 'F13'

	use &n0f13 in 0 alias f13 excl
	if .not. neterr()
		exit
	endif

	wait "Inhabilite terminal que usa Empresa: "+wxcoem wind nowait
enddo
sele f13
&& R.03b Mariano
*copy stru to &wmacro
copy stru to &d0f99.y
sele 0
use &d0f99.y in 0 alias 'f99y'
***vfp***
IF version()='Visual FoxPro'
	copy to &wmacro type fox2x
else
	copy to &wmacro
endif
***vfp***
use in f99y
erase &d0f99.y.dbf
sele f13
&& R.03e Mariano
use

wmacro=wdest+'14'       && Numeraciones
n0f14=wsour+'14'
do while .t.

	fieldvec[1] = 'F14'

	use &n0f14 in 0 alias f14 excl
	if .not. neterr()
		exit
	endif

	wait "Inhabilite terminal que usa Empresa: "+wxcoem wind nowait
enddo
sele f14
copy for d14sucu='0000' to &wmacro   && Copia las numeraciones de sucu 0000
use

wmacro=wdest+'15'       && Movimientos Auxiliares
n0f15=wsour+'15'
do while .t.

	fieldvec[1] = 'F15'

	use &n0f15 in 0 alias f15 excl
	if .not. neterr()
		exit
	endif

	wait "Inhabilite terminal que usa Empresa: "+wxcoem wind nowait
enddo
sele f15
&& R.03b Mariano
*copy stru to &wmacro
copy stru to &d0f99.y
sele 0
use &d0f99.y in 0 alias 'f99y'
***vfp***
IF version()='Visual FoxPro'
	copy to &wmacro type fox2x
else
	copy to &wmacro
endif
***vfp***
use in f99y
erase &d0f99.y.dbf
sele f15
&& R.03e Mariano
use

wmacro=wdest+'20'       && Categorกas
n0f20=wsour+'20'
do while .t.

	fieldvec[1] = 'F20'

	use &n0f20 in 0 alias f20 excl
	if .not. neterr()
		exit
	endif

	wait "Inhabilite terminal que usa Empresa: "+wxcoem wind nowait
enddo
sele f20
copy for d20clie>='99990' to &wmacro   && Copia los Clientes Eventuales
use

*-----> fin de copia de archivos

*------> inicializacion especiales

n0f00=wdest+'00'
do while .t.

	fieldvec[1] = 'F00'

	use &n0f00 in 0 alias f00 excl
	if .not. neterr()
		exit
	endif

	wait "Inhabilite terminal que usa Empresa: "+wxcoem wind nowait
enddo
sele f00
* ÛÛ !!!!!  OJO   Borrar todo lo que arrastra y jode

* Claves
pas = encript(spac(20),3)

repl P0CL1    with pas;
    ,P0CL2    with pas;
    ,P0CL3    with pas;
    ,P0CL4    with pas;
    ,P0CL5    with pas;
    ,P0EMP    with '';
    ,P0CUI    with '';
	,P0NUIB	  with '';
    ,P0DIR    with '';
    ,P0LOC    with '';
    ,P0COD    with '';
    ,P0TLP    with '';
    ,p0uavA   with '';
    ,p0uavB   with '';
    ,p0uavC   with '';
    ,p0ubvA   with '';
    ,p0ubvB   with '';
    ,p0ubvC   with '';
    ,p0ucvA   with '';
    ,p0ucvB   with '';
    ,p0ucvC   with '';
    ,P0UNFC   with '';
    ,P0UNCC   with '';
    ,P0UNDC   with '';
    ,P0JUCE   with '';
    ,P0IMFA   with 'N';
    ,P0COIM   with '1';
    ,P0MACC   with 'N';
    ,P0EMCG   with '';
    ,P0VIMP1  with '';
    ,P0VIMP2  with '';
    ,P0VIMP3  with '';
    ,P0VIMP4  with '';
    ,P0VIMP5  with '';
    ,P0VIMP6  with '';
    ,P0CIMP1  with '';
    ,P0CIMP2  with '';
    ,P0CIMP3  with '';
    ,P0CIMP4  with '';
    ,P0CIMP5  with '';
    ,P0CIMP6  with '';
    ,P0POCE   with 0;
    ,P0COMU   with 'N'
use

n0f14=wdest+'14'
do while .t.

	fieldvec[1] = 'F14'

	use &n0f14 in 0 alias f14 excl
	if .not. neterr()
		exit
	endif

	wait "Inhabilite terminal que usa Empresa: "+wxcoem wind nowait
enddo
sele f14
repl all d14ulnu with spac(8),d14n446 with spac(8)
use

* ฿Ü฿ Terminaciones Anormales ฿Ü฿

w_fi01=d0f01
w_fi02=d0f02
w_fi03=d0f03
w_fi04=d0f04
w_fi05=d0f05
w_fi06=d0f06
w_fi07=d0f07
w_fi08=d0f08
w_fi09=d0f09
w_fi13=d0f13
w_fi14=d0f14
w_fi15=d0f15
w_fi20=d0f20


d0f01=wdest+'01'
d0f02=wdest+'02'
d0f03=wdest+'03'
d0f04=wdest+'04'
d0f05=wdest+'05'
d0f06=wdest+'06'
d0f07=wdest+'07'
d0f08=wdest+'08'
d0f09=wdest+'09'
d0f13=wdest+'13'
d0f14=wdest+'14'
d0f15=wdest+'15'
d0f20=wdest+'20'

if .not. eof('f10')
	w_re10=recno('f10')
else
	w_re10=0
endif

sele f10
use

do db_indexiw with .f.

do while .t.
	fieldvec[1]='f10'

	use &d0f10 in 0 alias f10 order p0coem     && Empresas

	if .not. neterr()
		exit
	endif
enddo

sele f10
if .not. empty(w_re10)
	go w_re10 in f10
else
	=db_goeof('f10')
endif

if used('F98')
	sele f98
	use
endif

do while .t.
	fieldvec[1]='f98'

	use &d0f98 in 0 alias f98 order d98alia

	if .not. neterr()
		exit
	endif
enddo

if used('s02')
	sele s02
	use
endif

d0fs2=allt(m.p0path)+'ssf002.dbf'

d0f01=w_fi01
d0f02=w_fi02
d0f03=w_fi03
d0f04=w_fi04
d0f05=w_fi05
d0f06=w_fi06
d0f07=w_fi07
d0f08=w_fi08
d0f09=w_fi09
d0f13=w_fi13
d0f14=w_fi14
d0f15=w_fi15
d0f20=w_fi20

return

*-----------------------------------------------------------------------------
procedure  makeuser   &&  genera usuarios para esta empresa
*-----------------------------------------------------------------------------

do while .t.
	fieldvec[1] = 'f66'
	fieldvec[2] = 'S01'

	use &d0f66 in 0 alias f66 order d66user    && Usuarios por empresa
	use ssf001 in 0 alias s01 order 1          && Usuarios

	if .not. neterr()
		exit
	endif
enddo

sele s01

go top
do while !eof('s01')
	if !seek(s01.d1user+m.p0coem,'f66') and !s01.d1user='SUPERVISOR'
		sele f66
*		if rlock()	&& R.04 Mariano
*			append blank	&& R.04 Mariano
			=net_appe('f66')	&& R.04 Mariano
			replace d66user with s01.d1user
			replace d66empr with m.p0coem
*			unlock	&& R.04 Mariano
*		endif	&& R.04 Mariano
		sele s01
	endif
	skip in s01
enddo

sele f66
use
sele s01
use
sele alias (w_alias)

return

