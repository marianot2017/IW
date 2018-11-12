*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP009.PRG -> Abm de Facturas tipo x cliente
*)-H
*)-H Release		 : 01
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

w_pfile='09'
w_clave='m.d9clie+m.d9codi'
w_alias='f'+w_pfile
w_title=db_name(w_alias)

*) Apertura de archivos--------------------------------------------------------

fieldvec[1]='F00'
fieldvec[2]='F01'
fieldvec[3]='F02'
fieldvec[4]='F04'
fieldvec[5]='F08'
fieldvec[6]='F09'
fieldvec[7]='F20'

if !w_call

	use (d0f00) in 0 alias 'f00'					   &&(Parametros)
	use (d0f01) in 0 alias 'f01' order 'd1tip'   &&(Tablas)
	use (d0f02) in 0 alias 'f02' order 'd2clie'  &&(Clientes)
	use (d0f04) in 0 alias 'f04' order 'd4peri'  &&(Periodos)
	use (d0f08) in 0 alias 'f08' order 'd8codi'  &&(Facturas tipo)
	use (d0f09) in 0 alias 'f09' order 'd9clie'  &&(Facturas tipo x cliente)
	use (d0f20) in 0 alias 'f20' order 'd20tipo' &&(Tipos impos. x periodos)
	
	if neterr()
		pop key
		pop menu _msysmenu
		fieldvec=''
		=closall()
		return
	endif
	
endif

fieldvec=''

*)-----------------------------------------------------------------------------

select (w_alias)
scatter memvar blank

set skip of menu _msysmenu .t.
do ssm999.mpr

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
*	do brow09
    if version()='Visual FoxPro'
		*** cambio el lastkey() ***
		keyboard '{tab}'
		=inkey()
		DO wbrow WITH 'brow09()'
    else
   		do brow09
    endif
    ***vfp***
   if w_alta
      w_alta=.f.
      scatter memvar memo blank
      do iws009.spr
   endif

else 

	***vfp***
    if version()='Visual FoxPro'
	    on key label esc do actsalir
	endif
	***vfp***
	do iws009.spr
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
   set orde to 'd9clie' in (w_alias)
   if .not. empty(w_alan)
   	sele (w_alan)
   	w_alias=w_alan
   endif
   rele wind iws009
else
	rele wind iws009,barraabm
	=closall()
endif
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC Alta09
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
if w_call
	w_alta=.t.
   deac wind (w_title)
endif
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC Baja09
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
do clav09
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
PROC clav09
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
m.d9clie=strcero(m.d9clie)
m.d9codi=strcero(m.d9codi)
show get m.d9clie
show get m.d9codi

if w_recn<>0
	w_recn=recno(w_alias)
endif

w_orde=order(w_alias)
set orde to 'd9clie' in (w_alias)

if seek(w_pref+&w_clave,w_alias)

   if w_recn<>recno(w_alias) .or. .not. st_rlock(w_alias)
      do work09
      w_recn=recno(w_alias)
   endif
   if w_call
      unlock in (w_alias)
      clear read
   endif

else

	if w_recn<>recno(w_alias)
      =work09(.t.)
      w_recn=recno(w_alias)
   endif
   
endif

set orde to (w_orde) in (w_alias)
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC work09
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_eof

select (w_alias)
if !w_eof
	scatter memvar
else
	scatter memvar blank fields except d9clie,d9codi
endif

if w_rdlevel<>rdlevel() and iif(version()='Visual Fox', !w_call,.t.)	&& ***vfp***
	=valid09(.t.)
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
PROC when09
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
w_retu=.t.
w_vari=varread()
p_curobj=_curobj
=irclave('D9CODI','m.d9tipo')

if !(w_vari='D9CLIE') and !(lastkey()=p_f4) and w_retu
	w_show=.t.
	do fhelp with 'D9CLIE','','iwp002','f02','d2clie','d2nomb','Cliente inexistente...',(p_strc),'w_retu'
	if w_retu=.f.
		wait window 'Cliente inexistente...'
		_curobj=objnum(m.d9clie)
	endif
endif

return(w_retu)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC valid09
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_show

***vfp***
if version()='Visual Fox' and (w_pfile!='09' or (!wexist('IWS009') and wontop()!='IWS009')) and w_show
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

if (w_vari='D9CLIE' and lastkey()=p_f4) or w_show
	do fhelp with 'D9CLIE','','iwp002','f02','d2clie','d2nomb','Cliente inexistente...',(p_strc),'w_retu'
endif

if w_vari='D9CODI' or w_show
	do fhelp with 'D9CODI','','iwp008','f08','d8codi','d8desc','Factura tipo inexistente...',(p_strc),'w_retu'
endif

if w_vari='D9RUBR' or w_show
	do fhelp with 'D9RUBR','RV','iwp001','f01','d1cla','d1des','Rubro inexistente...',(p_strc),'w_retu'
endif

=ultdepri('D9CLIE','m.d9rubr')
=prideult('D9RUBR','m.d9rubr')
=modicampo('D9CLIE','D9CODI')
return(w_retu)

***vfp***
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc actu09
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	on key label f9 do actu09x
	keyboard '{f9}'
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc actu09x
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

if !valid09(.t.)
	return
endif

do clav09
if .not. eof(w_alias) .and. .not. st_rlock(w_alias)
   return 
endif

=db_capt(w_alias)
w_orde=order(w_alias)
set orde to 'd9clie' in (w_alias)

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
      do work09
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
PROC brow09
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
push menu _msysmenu
=skipbar('Registro','Insertar' ,!w_call)
=skipbar('Registro','Agregar'	 ,.t.)
=skipbar('Registro','Eliminar' ,.t.)
=skipbar('Registro','Modificar',.t.)
=skipbar('Registro','Consultar',.t.)
=skipbar('Registro','Elegir'	 ,.t.)
=skipbar('Registro','Impresi๓n',.t.)

w_orde=order(w_alias)
set order to tag 'd9desc' in (w_alias)
if eof(w_alias)
	go top in (w_alias)
endif

sele (w_alias)
set rela to d9clie into f02
set rela to d9codi into f08 additive
set rela to 'RV'+d9rubr into f01 additive

browse fields ;
	d9clie	  :h='C๓digo' :p='@K':v=fenter():f,;
	f02.d2nomb :h='Cliente':p='@K',;	
	d9codi 	  :h='C๓d'    :p='@K',;
	f08.d8desc :h='Factura':p='@K',;
	d9tipo 	  :h='Tipo'   :p='@K',;
	d9rubr 	  :h='RV'	  :p='@K',;
	f01.d1des  :h='Rubro'  :p='@K';
	nome noap noed node norm in screen title (w_title) font 'arial',10;
	valid :f iif(w_call,.t.,salebrow(Lastkey()))

if !w_call
	=work09()
endif

sele (w_alias)
set rela to

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
PROC busc09
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
w_orde=order(w_alias)
do busca
if .not. eof(w_alias)
	do work09
endif
set orde to (w_orde) in (w_alias)
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC orde09
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
do case 
   case prompt()='Ordenado por C๓digo'
      set orde to 'd9clie' in (w_alias)
   case prompt()='Ordenado por Descripci๓n'
     	set orde to 'd9desc' in (w_alias)
endcase
if wexist(w_title)
	show wind (w_title) refresh
endif
return
