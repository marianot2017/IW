*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP014.PRG -> Mantenimiento de numeraciones
*)-H
*)-H Release 		 : 01
*)-H Fecha de inicio : 22/12/1998.-
*)-H Fecha de fin    : 29/12/1998.-
*)-H
*)-H Actualizaciones
*)-H
*)-H Fecha       Programmer      Comentarios
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_pref,w_call

PRIVATE ALL LIKE w_*
if empty(parameters())
	w_call=.f.
	w_pref=''
endif

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

w_pfile='14'
w_clave='m.d14sucu+m.d14tifo'
w_alias='F'+w_pfile
w_title=ftable(w_pref,'title')

*) Apertura de archivos--------------------------------------------------------

fieldvec[1]='F14'

if .not. w_call

	use (d0f14) in 0 alias 'f14' order 'd14tico' &&(Numeraciones)
	
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
	
endif

fieldvec=''

*)-----------------------------------------------------------------------------

select (w_alias)
set filter to d14tico=w_pref
go top
scatter memvar blank

set skip of menu _msysmenu .t.
do ssm999.mpr &&(menu registro)

=skipbar('registro','insertar',.t.)
=skipbar('registro','Ordenado por C๓digo',.t.)
=skipbar('registro','Ordenado por Descripci๓n',.t.)
=skipbar('registro','Elegir',.t.)
if w_call
	=skipbar('registro','Eliminar' ,.t.)
	=skipbar('registro','Modificar',.t.)
endif

on key label f1 do nada
if w_agre
	on key label f9 keyboard '{ctrl+f9}'
endif

if w_call

    ***vfp***
*	do brow14
    if version()='Visual FoxPro'
		*** cambio el lastkey() ***
		keyboard '{tab}'
		=inkey()
		DO wbrow WITH 'brow14()'
    else
   		do brow14
    endif
    ***vfp***
   if w_alta
      w_alta=.f.
      scatter memvar blank
      do iws014.spr
   endif

else 

	***vfp***
    if version()='Visual FoxPro'
	    on key label esc do actsalir
	endif
	***vfp***
	do iws014.spr
	***vfp***
    if version()='Visual FoxPro'
	    on key label esc
	endif
	***vfp***

endif

pop key 
do ssm995.mpr	&& R.01 Mariano (puesto porque en vfp quedan activas los key del menu usado y de esta manera se limpian)
pop menu _msysmenu

if .not. empty(w_wind)
   acti wind (w_wind)
endif

if w_call
   set orde to 'd14tico' in (w_alias)
   if .not. empty(w_alan)
   	sele (w_alan)
   	w_alias=w_alan
   endif
   rele wind iws014
else
	rele wind iws014,barraabm
	=closall()
endif
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC Alta14
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
if w_call
	w_alta=.t.
   deac wind (w_title)
endif
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC Baja14
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
do clav14
if eof(w_alias) .or. .not. st_rlock(w_alias)
   return 
endif

if sino('Acepta eliminaci๓n ?.')='S'
	if begintran()
		=dele_rec(w_alias)
      unlock in (w_alias)
     	=endtran()
	endif
	do siguiente
endif
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC clav14
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
if w_recn<>0
   w_recn=recno(w_alias)
endif

w_orde=order(w_alias)
set orde to 'd14tico' in (w_alias)

if seek(w_pref+&w_clave,w_alias)
   if w_recn<>recno(w_alias) .or. .not. st_rlock(w_alias)
      do work14
      w_recn=recno(w_alias)
   endif
   if w_call
      unlock in (w_alias)
      clear read
   endif

else

	if w_recn<>recno(w_alias)
      =work14(.t.)
      w_recn=recno(w_alias)
   endif
   
endif

set orde to (w_orde) in (w_alias)
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC work14
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_eof

select (w_alias)
if !w_eof
	scatter memvar
else
	scatter memvar fields except d14tico,d14sucu,d14tifo
endif
m.d14tico=w_pref

if w_rdlevel<>rdlevel() and iif(version()='Visual Fox', !w_call,.t.)	&& ***vfp***
	=valid14(.t.)
	show gets
endif

if .not. eof(w_alias)
   =rlock(w_alias)
   if .not. st_rlock(w_alias,.t.) 
   	_curobj=1
   endif
endif
SHOW GETS
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC when14
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
w_retu=.t.
w_vari=varread()
p_curobj=_curobj
=irclave('D14TIFO','m.d14ulnu')
return(w_retu)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC valid14
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_show

***vfp***
if version()='Visual Fox' and (w_pfile!='14' or (!wexist('IWS014') and wontop()!='IWS014')) and w_show
	if lastkey()=p_esc
		return .t.
	else
		return .f.
	endif
endif
***vfp***

if empty(parameters())
	w_show=.f.
endif

w_retu=.t.
w_vari=varread()
w_vread='m.'+w_vari

if w_vari='D14SUCU' or w_show
	m.d14sucu=strcero(m.d14sucu)
	SHOW GETS
endif

*===>  VER VALIDACIONES SEGUN w_prog y cat. impositiva

if w_vari='D14TIFO' or w_show
	DO CASE
			CASE w_prog = "RETENCIONES"
						if !m.d14tifo$' '
								if !w_show
										wait wind 'Opciones disponibles:  .'
								endif
								w_retu=.f.
						endif
			OTHERWISE
					if !m.d14tifo$'ABCEMTZ '
							if !w_show
									wait wind 'Opciones disponibles: A,B,C,E,M,T,Z, .'
							endif
							w_retu=.f.
					endif
	ENDCASE
endif


if w_vari='D14ULNU' or w_show
	m.d14ulnu=strcero(m.d14ulnu)
	show get m.d14ulnu
endif

if w_vari='D14N446' or w_show
	m.d14n446=strcero(m.d14n446)
	show get m.d14n446
endif


if w_vari='D14VCAI' or w_show
	if empty(m.d14ncai) and !empty(m.d14vcai)
		if !w_show
			wait wind 'Falta ingresar N๚mero de C.A.I.'
		endif
		w_retu=.f.
	endif
	if !empty(m.d14ncai) and empty(m.d14vcai)
		if !w_show
			wait wind 'Falta ingresar Vencimiento de C.A.I.'
		endif
		w_retu=.f.
	endif
endif

if w_vari='D14DCAI' or w_show
	if empty(m.d14ncai) and !empty(m.d14dcai)
		if !w_show
			wait wind 'Falta ingresar N๚mero de C.A.I.'
		endif
		w_retu=.f.
	endif
	if empty(m.d14vcai) and !empty(m.d14dcai)
		if !w_show
			wait wind 'Falta ingresar Vencimiento de C.A.I.'
		endif
		w_retu=.f.
	endif
	if !empty(m.d14dcai)
		w_dcai=vercai(m.d14ncai)
		if m.d14dcai!=w_dcai
			if !w_show
				wait wind 'Error en N๚mero de C.A.I. o digito verificador = '+w_dcai
			endif
			w_retu=.f.
		endif
	else
		if !empty(m.d14ncai)
			if !w_show
				wait wind 'Falta ingresar Digito verificador de C.A.I.'
			endif
			w_retu=.f.
		endif
	endif
endif

=ultdepri('D14SUCU','m.d14dcai')
=prideult('D14DCAI','m.d14dcai')
=modicampo('D14TICO','D14SUCU','D14TIFO')
SHOW GETS
return(w_retu)

***vfp***
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc actu14
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	on key label f9 do actu14x
	keyboard '{f9}'
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc actu14x
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
if w_agre
	on key label f9 keyboard '{ctrl+f9}'
else
	on key label f9
endif
***vfp***

if !w_agre .and. !w_modi
    return
endif

if !valid14(.t.)
   wait 'Debe completar datos...' wind
	return
endif

do clav14
if .not. eof(w_alias) .and. .not. st_rlock(w_alias)
   return 
endif

=db_capt(w_alias)
w_orde=order(w_alias)
set orde to 'd14tico' in (w_alias)

if .not. seek(w_pref+&w_clave,w_alias)

	if w_agre
      wait 'Agregando registro...' wind time .4
		if begintran()
	      =net_appe(w_alias)
	      sele (w_alias)
   	   gather memvar 
   	   =endtran()
   	endif
      w_modicampo=.f.
      =db_rlock(w_alias) 
      if .not. w_call
         _curobj=1
      endif
   endif

else 

   if st_rlock(w_alias)
      if w_modi 
         wait 'Modificando registro...' wind time .4
         sele (w_alias)
			if begintran()
				sele (w_alias)
				gather memvar
   	      =endtran()
   	   endif
         w_modicampo=.f.
      endif 
   else 
      do work14
   endif
   
endif
unlock in f97

if w_call
   unlock in (w_alias)
   clear read
endif

set order to (w_orde) in (w_alias)
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC brow14
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
push menu _msysmenu
=skipbar('Registro','Insertar' ,!w_call)
=skipbar('Registro','Agregar'  ,.t.)
=skipbar('Registro','Eliminar' ,.t.)
=skipbar('Registro','Modificar',.t.)
=skipbar('Registro','Consultar',.t.)
=skipbar('Registro','Elegir'   ,.t.)

w_orde=order(w_alias)
*set orde to 'd2nomb' in (w_alias)
if eof(w_alias)
	go top in (w_alias)
endif

sele (w_alias)
browse fields ;
	d14sucu :h='SU':p='@K':v=fenter():f,;
	d14tifo :h='TF':p='@K',;
	d14ulnu :h='Ult.Cpte.',;
	d14n446 :h='Ult.446';
	nome noap noed node norm in screen title (w_title) font 'arial',10;
	valid :f iif(w_call,.t.,salebrow(Lastkey()))

if !w_call
	=work14()
endif

set order to (w_orde) in (w_alias)
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

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC busc14
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
w_orde=order(w_alias)
do busca
if .not. eof(w_alias)
	do work14
endif
set orde to (w_orde) in (w_alias)
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC orde14
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
do case 
   case prompt()='Ordenado por C๓digo'
      set orde to 'd14tico' in (w_alias)
   case prompt()='Ordenado por Descripci๓n'
     	set orde to 'd2nomb' in (w_alias)
endcase
if wexist(w_title)
	show wind (w_title) refresh
endif
return
