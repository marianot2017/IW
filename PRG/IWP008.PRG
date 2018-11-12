*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP008.PRG -> Abm de Facturas tipo
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

w_pfile='08'
w_clave='m.d8codi'
w_alias='f'+w_pfile
w_title=db_name(w_alias)

w_btnok=0

*) Apertura de archivos--------------------------------------------------------

fieldvec[1]='F00'
fieldvec[2]='F08'

if !w_call

	use (d0f00) in 0 alias 'f00'					  &&(Parametros)
	use (d0f08) in 0 alias 'f08' order 'd8codi' &&(Facturas tipo)
	
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

w_ocv1=f00->p0ocv1
w_ocv2=f00->p0ocv2
w_ocv3=f00->p0ocv3

select (w_alias)
scatter memvar memo blank

set skip of menu _msysmenu .t.
do iwm008.mpr

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
*	do brow08
    if version()='Visual FoxPro'
		*** cambio el lastkey() ***
		keyboard '{tab}'
		=inkey()
		DO wbrow WITH 'brow08()'
    else
   		do brow08
    endif
    ***vfp***
   if w_alta
      w_alta=.f.
      scatter memvar memo blank
      do iws008.spr
   endif

else 

	***vfp***
    if version()='Visual FoxPro'
	    on key label esc do actsalir
	endif
	***vfp***
	do iws008.spr
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
   set orde to 'd8codi' in (w_alias)
   if .not. empty(w_alan)
   	sele (w_alan)
   	w_alias=w_alan
   endif
   rele wind iws008
else
	rele wind iws008,barraabm
	=closall()
endif
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC Alta08
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
if w_call
	w_alta=.t.
   deac wind (w_title)
endif
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC Baja08
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
do clav08
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
PROC clav08
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
if w_recn<>0
	w_recn=recno(w_alias)
endif

w_orde=order(w_alias)
set orde to 'd8codi' in (w_alias)

if seek(w_pref+&w_clave,w_alias)

   if w_recn<>recno(w_alias) .or. .not. st_rlock(w_alias)
      do work08
      w_recn=recno(w_alias)
   endif
   if w_call
      unlock in (w_alias)
      clear read
   endif

else

	if w_recn<>recno(w_alias)
      =work08(.t.)
      w_recn=recno(w_alias)
   endif
   
endif

set orde to (w_orde) in (w_alias)
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC work08
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_eof

select (w_alias)
if !w_eof
	scatter memvar memo
else
	scatter memvar memo blank fields except d8codi
endif

if w_rdlevel<>rdlevel() and iif(version()='Visual Fox', !w_call,.t.)	&& ***vfp***
	=valid08(.t.)
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
PROC when08
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
w_retu=.t.
w_vari=varread()
p_curobj=_curobj
=irclave('D8CODI','m.d8desc')
return(w_retu)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC valid08
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_show

***vfp***
if version()='Visual Fox' and (w_pfile!='08' or (!wexist('IWS008') and wontop()!='IWS008')) and w_show
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

if w_vari='D8CODI' or w_show
	m.d8codi=strcero(m.d8codi)
	show get m.d8codi
endif

m.d8tota=m.d8imp1+m.d8imp2+m.d8imp3+m.d8imp4+m.d8imp5+m.d8imp6+m.d8imp7+m.d8imp8
show get m.d8tota

if !(w_vari='D8CODI') and w_show
	if empty(m.d8tota)
		wait wind 'Total NULO.'
		w_retu=.f.
	endif
endif

=ultdepri('D8CODI','m.d8imp8')
=prideult('D8IMP8','m.d8imp8')
=modicampo('D8CODI')
return(w_retu)

***vfp***
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc actu08
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	on key label f9 do actu08x
	keyboard '{f9}'
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc actu08x
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

if !valid08(.t.)
	return
endif

do clav08
if .not. eof(w_alias) .and. .not. st_rlock(w_alias)
   return 
endif

=db_capt(w_alias)
w_orde=order(w_alias)
set orde to 'd8codi' in (w_alias)

if .not. seek(w_pref+&w_clave,w_alias)

	if w_agre
      wait 'Agregando registro...' wind time .4
		if begintran()
	      =net_appe(w_alias)
	      sele (w_alias)
   	   gather memvar memo
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
				gather memvar memo
   	      =endtran()
   	   endif
         w_modicampo=.f.
      endif 
   else 
      do work08
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
PROC brow08
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
set order to tag 'd8desc' in f08
if eof(w_alias)
	go top in (w_alias)
endif

sele (w_alias)
browse fields ;
	d8codi :h='C๓d'		  :p='@K':v=fenter():f,;
	d8desc :h='Descripci๓n':p='@K',;
	d8tota :h='Total'		  :p='@K',;
	d8imp1 :h='Importe 1'  :p='@K',;
	d8imp2 :h='Importe 2'  :p='@K',;
	d8imp3 :h='Importe 3'  :p='@K',;
	d8imp4 :h='Importe 4'  :p='@K',;
	d8imp5 :h='Importe 5'  :p='@K',;
	d8imp6 :h='Importe 6'  :p='@K',;
	d8imp7 :h='Importe 7'  :p='@K',;
	d8imp8 :h='Importe 8'  :p='@K';
	nome noap noed node norm in screen title (w_title) font 'arial',10;
	valid :f iif(w_call,.t.,salebrow(Lastkey()))

if !w_call
	=work08()
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
PROC busc08
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
w_orde=order(w_alias)
set order to tag 'd8desc' in f08
do busca
if .not. eof(w_alias)
	do work08
endif
set orde to (w_orde) in (w_alias)
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC orde08
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
do case 
   case prompt()='Ordenado por C๓digo'
      set orde to 'd8codi' in (w_alias)
   case prompt()='Ordenado por Descripci๓n'
     	set orde to 'd8desc' in (w_alias)
endcase
if wexist(w_title)
	show wind (w_title) refresh
endif
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC text08
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
DO iws008t.spr
return
