*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP024.PRG -> Mantenimiento Percep./Retenc./Otros
*)-H
*)-H Release		 : 02
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

w_pfile='24'
w_clave='m.d24codi+m.d24tipo'
w_alias='F'+w_pfile
w_title=db_name(w_alias)

*) Apertura de archivos--------------------------------------------------------

fieldvec[1]='F24'

if !used('f01')
*	use (d0f01) in 0 alias 'f01' order 'd1tip' &&(Tablas)		

	use (d0f21) in 0 alias 'f21' order 'd21tip' &&(Tablas)
endif	

if !w_call

	use (d0f24) in 0 alias 'f24' order 'd24codi' &&(Percep./Retenc./Otros)
	
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
ELSE
			IF (WONTOP() = 'IWS005C1' and m.d5tico = "7")							OR		;		
				(WONTOP() = 'IWS005V1' and m.d5tico = "O")		&& R.01 Mariano
								w_title	= "Percep./Retenc. de IVA / Ganancias"
			ELSE
				w_title	= "Percepciones de IVA / Ganancias"
			ENDIF

		
endif

=creatipos('V')	&& perc/ret/otros

fieldvec=''

*)-----------------------------------------------------------------------------

select (w_alias)
scatter memvar blank

w_refe=''

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
*	do brow24
    if version()='Visual FoxPro'
		*** cambio el lastkey() ***
		keyboard '{tab}'
		=inkey()
		DO wbrow WITH 'brow24()'
    else
   		do brow24
    endif
    ***vfp***
   if w_alta
      w_alta=.f.      
      do iws024.spr
   endif
   
else

	***vfp***
    if version()='Visual FoxPro'
	    on key label esc do actsalir
	endif
	***vfp***
	do iws024.spr
	***vfp***
    if version()='Visual FoxPro'
	    on key label esc
	endif
	***vfp***
	
endif

pop key
do ssm995.mpr	&& R.02 Mariano (puesto porque en vfp quedan activas los key del menu usado y de esta manera se limpian)
pop menu _msysmenu

if .not. empty(w_wind)
	acti wind (w_wind)
endif

if w_call
   set orde to 'd24codi' in (w_alias)
   if .not. empty(w_alan)
   	sele (w_alan)
   	w_alias=w_alan
   endif
   rele wind iws024
else
	rele wind iws024,barraabm
	=closall()
	=borratipos()
endif
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC Alta24
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
if w_call
	w_alta=.t.
   deac wind (w_title)
endif
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC Baja24
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PRIVATE w_msg
w_msg='Acepta eliminaci๓n ?.'

do clav24
if eof(w_alias) .or. .not. st_rlock(w_alias)
	return
endif

if sino(w_msg)='S'
	if begintran()
		=dele_rec(w_alias)
		unlock in (w_alias)
		=endtran()
	endif
	do siguiente
endif
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC clav24
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
if w_recn<>0
	w_recn=recno(w_alias)
endif

w_orde=order(w_alias)
set orde to 'd24codi' in (w_alias)

if seek(w_pref+&w_clave,w_alias)

	if w_recn<>recno(w_alias) .or. .not. st_rlock(w_alias)
      do work24
      w_recn=recno(w_alias)
   endif
   if w_call
      unlock in (w_alias)
      clear read
   endif

else

	if w_recn<>recno(w_alias)
      =work24(.t.)
      w_recn=recno(w_alias)
   endif
   
endif

set orde to (w_orde) in (w_alias)
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC work24
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_eof

select (w_alias)
if !w_eof
	scatter memvar
else
	scatter memvar blank fields except d24codi,d24tipo
endif

if w_rdlevel<>rdlevel() and iif(version()='Visual Fox', !w_call,.t.)	&& ***vfp***
	=valid24(.t.)
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
PROC when24
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
w_retu=.t.
w_vari=varread()
p_curobj=_curobj
=irclave('D24TIPO','m.d24desc')

return(w_retu)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC valid24
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_show

***vfp***
if version()='Visual Fox' and (w_pfile!='24' or (!wexist('IWS024') and wontop()!='IWS024')) and w_show
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

if w_vari='D24CODI' or w_show
	m.d24codi=strcero(m.d24codi)
	show get m.d24codi
	show get m.d24tipo	
endif

if w_vari='D24TIPO' or w_show
	if !m.d24tipo$'IGO '
		w_retu=.f.
	endif
endif

if w_vari='D24REFE' or w_show
	do case
		case m.d24tipo='I'
			do fhelp with 'D24REFE','PR','iwp021','f21','d21cla','d21des','Perc./Ret./Otros inexistente',(p_char),'w_retu'
		case m.d24tipo='G'		
			do fhelp with 'D24REFE','PG','iwp021','f21','d21cla','d21des','Perc./Ret./Otros inexistente',(p_char),'w_retu'
		otherwise
			do fhelp with 'D24REFE','OT','iwp021','f21','d21cla','d21des','Perc./Ret./Otros inexistente',(p_char),'w_retu'						
	endcase			
	show get m.d24refe
endif

=ultdepri('D24CODI','m.d24refe')
=prideult('D24REFE','m.d24refe')
=modicampo('D24CODI','D24TIPO')
return(w_retu)

***vfp***
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc actu24
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	on key label f9 do actu24x
	keyboard '{f9}'
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc actu24x
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

if !valid24(.t.)
   wait 'Debe completar datos...' wind
	return
endif

do clav24
if .not. eof(w_alias) .and. .not. st_rlock(w_alias)
   return 
endif

=db_capt(w_alias)
w_orde=order(w_alias)
set orde to 'd24codi' in (w_alias)
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
      do work24
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
PROC brow24
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
push menu _msysmenu
=skipbar('Registro','Insertar' ,!w_call)
=skipbar('Registro','Agregar'	 ,.t.)
=skipbar('Registro','Eliminar' ,.t.)
=skipbar('Registro','Modificar',.t.)
=skipbar('Registro','Consultar',.t.)
=skipbar('Registro','Elegir'	 ,.t.)

w_orde=order(w_alias)
set order to tag 'd24codi' in (w_alias)
if eof(w_alias)
	go top in (w_alias)
endif

sele (w_alias)
browse fields;
	d24codi :h='C๓digo'	 :p='@K':v=fenter():f,;
	d24tipo :h='Tipo' :p='@K',;
	d24desc :h='Descripci๓n':p='@K',;
	refe	= 	iif(d24tipo = "I" AND seek('PR'+d24refe,'f21'),f21.d21des,				;
				iif(d24tipo = "G" AND seek('PG'+d24refe,'f21'),f21.d21des,				;
				iif(d24tipo = "O" AND seek('OT'+d24refe,'f21'),f21.d21des, " ")))		; 
	:h='Referencia' :p='@k' ;
	nome noap noed node norm in screen title (w_title) font 'arial',10;
	valid :f iif(w_call,.t.,salebrow(Lastkey()))

if !w_call
	=work24()
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
PROC busc24
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
do busca
if .not. eof(w_alias)
	do work24
endif
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC orde24
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
do case 
   case prompt()='Ordenado por C๓digo'
      set orde to 'd24codi' in (w_alias)
   case prompt()='Ordenado por Descripci๓n'
     	set orde to 'd24desc' in (w_alias)
endcase
if wexist(w_title)
	show wind (w_title) refresh
endif
return
