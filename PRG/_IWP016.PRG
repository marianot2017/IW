*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP016.PRG -> Mantenimiento def.cabecera
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
	w_pref=''
	w_call=.f.
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

w_pfile='16'
w_clave='m.d16defi'
w_alias='F'+w_pfile
w_title=db_name(w_alias)

*) Apertura de archivos--------------------------------------------------------

fieldvec[1]='F16'
fieldvec[2]='F17'

if !w_call

	use (d0f16) in 0 alias 'f16' order 'd16defi' &&(Def.Cabezera)
	use (d0f17) in 0 alias 'f17' order 'd17defi' &&(Def.Renglones)
	
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
scatter memvar blank

set skip of menu _msysmenu .t.
do ssm999.mpr	&&(menu registro)

=skipbar('registro','insertar',.t.)
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
*	do brow16
    if version()='Visual FoxPro'
		*** cambio el lastkey() ***
		keyboard '{tab}'
		=inkey()
		DO wbrow WITH 'brow16()'
    else
   		do brow16
    endif
    ***vfp***
   if w_alta
      w_alta=.f.      
      do iws016.spr
   endif
   
else

	***vfp***
    if version()='Visual FoxPro'
	    on key label esc do actsalir
	endif
	***vfp***
	do iws016.spr
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
   set orde to 'd16defi' in (w_alias)
   if .not. empty(w_alan)
   	sele (w_alan)
   	w_alias=w_alan
   endif
   rele wind iws016
else
	rele wind iws016,barraabm
	=closall()
endif
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC Alta16
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
if w_call
	w_alta=.t.
   deac wind (w_title)
endif
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC Baja16
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PRIVATE w_msg
w_msg='Acepta eliminaci๓n ?.'

if seek(m.d16defi,'f17')
	w_msg=w_msg+' Al BORRAR esta definici๓n BORRARA TAMBIEN todos los renglones asociados a ella...'
endif

do clav16
if eof(w_alias) .or. .not. st_rlock(w_alias)
	return
endif

if sino(w_msg)='S'
	if begintran()
		=dele_rec(w_alias)
		unlock in (w_alias)
		if seek(m.d16defi,'f17')
			=db_flock('f17')
			sele f17
			delete while !eof() and d17defi=m.d16defi
			unlock in f17
			sele (w_alias)
		endif
		=endtran()
	endif
	do siguiente
endif
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC clav16
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
if w_recn<>0
	w_recn=recno(w_alias)
endif

w_orde=order(w_alias)
set orde to 'd16defi' in (w_alias)

if seek(w_pref+&w_clave,w_alias)

	if w_recn<>recno(w_alias) .or. .not. st_rlock(w_alias)
      do work16
      w_recn=recno(w_alias)
   endif
   if w_call
      unlock in (w_alias)
      clear read
   endif

else

	if w_recn<>recno(w_alias)
      =work16(.t.)
      w_recn=recno(w_alias)
   endif
   
endif

set orde to (w_orde) in (w_alias)
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC work16
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_eof

select (w_alias)
if !w_eof
	scatter memvar
else
	scatter memvar blank fields except d16defi
endif

if w_rdlevel<>rdlevel() and iif(version()='Visual Fox', !w_call,.t.)	&& ***vfp***
	=valid16(.t.)
	show gets
endif

if .not. eof(w_alias)
	=rlock(w_alias)
   if .not. st_rlock(w_alias,.t.)
   	_curobj=1
   endif
endif
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC when16
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
w_retu=.t.
w_vari=varread()
p_curobj=_curobj
=irclave('D16DEFI','m.d16nomb')
return(w_retu)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC valid16
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_show

***vfp***
if version()='Visual Fox' and (w_pfile!='16' or (!wexist('IWS016') and wontop()!='IWS016')) and w_show
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

if w_vari='D16DEFI' or w_show
	m.d16defi=strcero(m.d16defi)
	show get m.d16defi
endif

=ultdepri('D16DEFI','m.d16ley2')
=prideult('D16LEY2','m.d16ley2')
=modicampo('D16DEFI')
return(w_retu)

***vfp***
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc actu16
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	on key label f9 do actu16x
	keyboard '{f9}'
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc actu16x
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

if !valid16(.t.)
   wait 'Debe completar datos...' wind
	return
endif

do clav16
if .not. eof(w_alias) .and. .not. st_rlock(w_alias)
   return 
endif

=db_capt(w_alias)
w_orde=order(w_alias)
set orde to 'd16defi' in (w_alias)
if !seek(w_pref+&w_clave,w_alias)

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
      do work16
   endif
   
endif
unlock in f98

if w_call
   unlock in (w_alias)
   clear read
endif

set order to (w_orde) in (w_alias)
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC brow16
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
push menu _msysmenu
=skipbar('Registro','Insertar' ,!w_call)
=skipbar('Registro','Agregar'	 ,.t.)
=skipbar('Registro','Eliminar' ,.t.)
=skipbar('Registro','Modificar',.t.)
=skipbar('Registro','Consultar',.t.)
=skipbar('Registro','Elegir'	 ,.t.)

w_orde=order(w_alias)
set order to tag 'd16nomb' in (w_alias)
if eof(w_alias)
	go top in (w_alias)
endif

sele (w_alias)
browse fields;
	d16defi :h='Def'	 :p='@K':v=fenter():f,;
	d16nomb :h='Nombre':p='@K';
	nome noap noed node norm in screen title (w_title) font 'arial',10;
	valid :f iif(w_call,.t.,salebrow(Lastkey()))

if !w_call
	=work16()
endif

set order to tag (w_orde) in (w_alias)
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
PROC busc16
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
w_orde=order(w_alias)
set order to tag 'd16nomb' in (w_alias)
do busca
if .not. eof(w_alias)
	do work16
endif
set orde to (w_orde) in (w_alias)
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC orde16
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
do case 
   case prompt()='Ordenado por C๓digo'
      set orde to 'd16defi' in (w_alias)
   case prompt()='Ordenado por Descripci๓n'
     	set orde to 'd16nomb' in (w_alias)
endcase
if wexist(w_title)
	show wind (w_title) refresh
endif
return
