*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : CGP001C.PRG -> Mantenimiento de c.contables
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

w_pfile='01c'
w_clave='m.d1cuen'
w_alias='F'+w_pfile
w_title='CUENTAS CONTABLES'

*) Apertura de archivos--------------------------------------------------------

fieldvec[1]='F01c'

if !w_call

	ncf01=w0fipre+'01'
	use (ncf01) in 0 alias f01c order tag 'd1cuen'
	
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
*	do brow01c
    if version()='Visual FoxPro'
		*** cambio el lastkey() ***
		keyboard '{tab}'
		=inkey()
		DO wbrow WITH 'brow01c()'
    else
   		do brow01c
    endif
    ***vfp***
   if w_alta
      w_alta=.f.
      scatter memvar blank
      do cgs001c.spr
   endif

else

	***vfp***
    if version()='Visual FoxPro'
	    on key label esc do actsalir
	endif
	***vfp***
	do cgs001c.spr
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
   set orde to 'd1cuen' in (w_alias)
   if .not. empty(w_alan)
   	sele (w_alan)
   	w_alias=w_alan
   endif
   rele wind cgs001c
else
	rele wind cgs001c,barraabm
	=closall()
endif
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC alta01c
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
if w_call
	w_alta=.t.
   deac wind (w_title)
   keyboard '{enter}'	&& R.01 Mariano
endif
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC Baja01c
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
do clav01c
if eof(w_alias) .or. .not. st_rlock(w_alias)
   return 
endif

if errobaj()
	return
endif

if sino('Acepta eliminaci๓n ?.')='S'
	if begintran()
		w1padr=cpadre(m.d1cuen)
      if !empty(w1padr)
      	sele (w_alias)
      	wpunt=recno(w_alias)
         =seek(w1padr,'f01c')
         =db_rlock('f01c')
         replace d1hijo with d1hijo-1
         unlock in f01c
         go (wpunt) in (w_alias)
         =db_rlock('f01c')
		endif
      sele (w_alias)
		=dele_rec('f01c')
		unlock in f01c
		=endtran()				
	endif
	do siguiente
endif
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC clav01c
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
if w_recn<>0
   w_recn=recno(w_alias)
endif

w_orde=order(w_alias)
set orde to 'd1cuen' in (w_alias)

if seek(w_pref+&w_clave,w_alias)

   if w_recn<>recno(w_alias) .or. .not. st_rlock(w_alias)
      do work01c
      w_recn=recno(w_alias)
   endif
   if w_call
      unlock in (w_alias)
      clear read
   endif

else

	if w_recn<>recno(w_alias)
		w_d1cuen=m.d1cuen
      do work01c
      w_recn=recno(w_alias)
		m.d1cuen=w_d1cuen
   	show get m.d1cuen
   endif
   
endif

set orde to (w_orde) in (w_alias)
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC work01c
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
select (w_alias)
scatter memvar

if w_rdlevel<>rdlevel() and iif(version()='Visual Fox', !w_call,.t.)	&& ***vfp***
	=valid01c(.t.)
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
PROC when01c
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
w_retu=.t.
w_vari=varread()
p_curobj=_curobj
=irclave('D1CUEN','m.d1desc')

if !(w_vari='D1CUEN') and !(lastkey()=p_enter) and w_retu
	if empty(m.d1cuen)
		w_retu=.f.
		_curobj=objnum(m.d1cuen)
	endif
endif

return(w_retu)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC valid01c
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_show

***vfp***
if version()='Visual Fox' and (w_pfile!='01c' or (!wexist('CGS001C') and wontop()!='CGS001C')) and w_show
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

if (w_vari='D1CUEN' and lastkey()=p_enter) or w_show
	if empty(m.d1cuen)
		if !w_show
			wait wind 'C๓digo de cuenta invแlido...'
		endif
		w_retu=.f.
	endif
endif

if w_vari='D1DESC' or w_show
	if empty(m.d1desc)
		if !w_show
			wait wind 'Debe especificar la descripci๓n de la cuenta...'
		endif
		w_retu=.f.
	endif
endif

=ultdepri('D1CUEN','m.d1desc')
=prideult('D1DESC','m.d1desc')
=modicampo('D1CUEN')
return(w_retu)

***vfp***
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc actu01c
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	on key label f9 do actu01c0
	keyboard '{f9}'
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc actu01c0
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

if !valid01c(.t.)
   wait 'Debe completar datos...' wind
	return
endif

do clav01c
if .not. eof(w_alias) .and. .not. st_rlock(w_alias)
   return 
endif

=db_capt(w_alias)
w_orde=order(w_alias)
set orde to 'd1cuen' in (w_alias)

if !seek(w_pref+&w_clave,w_alias)

	if w_agre
	
		w1padr=cpadre(m.d1cuen)
      if erroalt()
      	return
      endif

      if begintran()
      	wait 'Agregando registro...' wind time .4
	      =db_flock('f01c')
   	   if !seek(w_pref+&w_clave,'f01c')
 	   	  =net_appe(w_alias)
 	   	  sele (w_alias)
	        gather memvar
	      endif
      	unlock in (w_alias)
			if !empty(w1padr)
   	   	=seek(w1padr,'f01c')
      		=db_rlock('f01c')
      		sele f01c
         	replace d1hijo with d1hijo+1
	         unlock in f01c
   	   endif   	   
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
				gather memvar
   	      =endtran()
   	   endif
         w_modicampo=.f.
      endif 
   else 
      do work01c
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
PROC brow01c
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
activate screen

push menu _msysmenu

=skipbar('Registro','Insertar' ,iif('CG\'$d0paco,.t.,!w_call))
=skipbar('Registro','Agregar'	 ,.t.)
=skipbar('Registro','Eliminar' ,.t.)
=skipbar('Registro','Modificar',.t.)
=skipbar('Registro','Consultar',.t.)
=skipbar('Registro','Elegir'	 ,.t.)

w_orde=order(w_alias)
set orde to 'd1cuen' in (w_alias)

if eof(w_alias)
	go top in (w_alias)
endif

sele (w_alias)
browse fields;
	d1cuen :h='Cuenta':p='@R '+d0pict :v=fenter():f,;
	w_impu=iif(empty(d1hijo),'*',' ') :h='',;
	d1desc :h='Descripci๓n';
	nome noap noed node norm in screen title w_title font 'arial',10;
	valid :f iif(w_call,.t.,salebrow(Lastkey()))
if !w_call
	=work01c()
endif

set order to (w_orde) in (w_alias)
pop menu _msysmenu
return

* อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC busc01c
* อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

w_orde=order(w_alias)
 
do busca

if .not. eof(w_alias)
   do work01c
endif
set orde to w_orde in (w_alias)
return 

* อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC Orde01c
* อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

do case 
   case prompt()='Ordenado por C๓digo'
      set orde to 1 in (w_alias)
   case prompt()='Ordenado por Descripci๓n'
      set orde to 2 in (w_alias)
endcase

if wexist(w_title)
   show wind (w_title) refresh
endif

return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC Erroalt
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
private wpunt,wretu
wretu=.f.
wpunt=iif(!eof('f01c'),recno('f01c'),0)

if .not. empty(w1padr)
   if !seek(w1padr,'f01c')
   	wait wind 'Cuenta sin Padre.'
      wretu=.t.
   endif
endif
if !empty(wpunt)
	go (wpunt) in f01c
endif
return(wretu)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC Errobaj
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
private wretu
wretu=.f.

if f01c.d1hijo>0
   wait wind 'Cuenta con Hijas...'
   wretu=.t.
endif
return(wretu)

*-----------------------------------------------------------------
proc Camp01c
*-----------------------------------------------------------------
if opcion$'MC'
	If d1hijo=0
   	@6,18 say 'Cuenta Imputable   '
   else
      @6,18 say 'Cuenta no Imputable'
   endif
   @ 07 ,18 get W1DESC pict '@k'  && valid !erro01()
endif
return
