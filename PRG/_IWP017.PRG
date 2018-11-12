*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP017.PRG -> Mantenimiento def.renglones
*)-H Release 		 : 02
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
wStatus=17

*) Datos del archivo a mantener------------------------------------------------

w_pfile='17'
w_clave='m.d17defi+m.d17orde'
w_alias='F'+w_pfile
w_title=db_name(w_alias)

*) Apertura de archivos--------------------------------------------------------

fieldvec[ 1]='F01'
fieldvec[ 2]='F02'
fieldvec[ 3]='F03'
fieldvec[ 4]='F05'
fieldvec[ 5]='F06'
fieldvec[ 6]='F15'
fieldvec[ 7]='F16'
fieldvec[ 8]='F17'
fieldvec[ 9]='F18'
fieldvec[10]='F21'
fieldvec[11]='F23'
fieldvec[12]='F24'

if !w_call

	use (d0f01) in 0 alias 'f01' order 'd1tip'   		&&(Tablas)
	use (d0f02) in 0 alias 'f02' order 'd2clie'			&&(Clientes)
	use (d0f03) in 0 alias 'f03' order 'd3prov'       	&&(Proveedores)
	use (d0f05) in 0 alias 'f05' order 'd5peri'       	&&(Comprobantes)
	use (d0f06) in 0 alias 'f06' order 'd6inte'       	&&(Arch.Alternativo)
	use (d0f15) in 0 alias 'f15' order 'd15peri'		&&(Mov.Complementarios)
	use (d0f16) in 0 alias 'f16' order 'd16defi' EXCL	&&(Def.Cabezera)
	use (d0f17) in 0 alias 'f17' order 'd17defi' EXCL	&&(Def.Renglones)
	use (d0f18) in 0 alias 'f18'
	use (d0f21) in 0 alias 'f21' order 'd21tip'		  	&&(Tablas)
	use (d0f23) in 0 alias 'f23' order 'd23codi' &&(Alicuotas)
	use (d0f24) in 0 alias 'f24' order 'd24codi' &&(Perc./Ret./Otros)
	
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

=creatipos('C')	&& clase de operacion

fieldvec=''

*)-----------------------------------------------------------------------------

select f05
go bott
if !eof('f05')
	skip
endif

perio=f05->d5peri
rubro=f05->d5rubr
clipr=f05->d5clie
juris=f05->d5juri
formu=f05->d5tifo

netgr=f05->d5imp1
nogra=f05->d5imp2
ivain=f05->d5imp4
ivani=f05->d5imp5
ivapr=f05->d5imp3
otco1=f05->d5imp6
otco2=f05->d5imp7
otco3=f05->d5imp8
impre=f05.d5imre
pedes=perio
pehas=perio

select (w_alias)
scatter memvar blank

set skip of menu _msysmenu .t.
do iwm017.mpr &&(Menu ayuda)

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
*	do brow17
    if version()='Visual FoxPro'
		*** cambio el lastkey() ***
		keyboard '{tab}'
		=inkey()
		DO wbrow WITH 'brow17()'
    else
   		do brow17
    endif
    ***vfp***
   if w_alta
      w_alta=.f.      
      do iws017.spr
   endif
   
else

	***vfp***
    if version()='Visual FoxPro'
	    on key label esc do actsalir
	endif
	***vfp***
	do iws017.spr
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
   set orde to 'd17defi' in (w_alias)
   if .not. empty(w_alan)
   	sele (w_alan)
   	w_alias=w_alan
   endif
   rele wind iws017
else
	rele wind iws017,barraabm
	=closall()
	=borratipos()
endif
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC Alta17
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
if w_call
	w_alta=.t.
   deac wind (w_title)
endif
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC Baja17
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
do clav17
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
PROC clav17
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
m.d17defi=strcero(m.d17defi)
m.d17orde=strcero(m.d17orde)
show get m.d17defi
show get m.d17orde

if w_recn<>0
	w_recn=recno(w_alias)
endif

w_orde=order(w_alias)
set orde to 'd17defi' in (w_alias)

if seek(w_pref+&w_clave,w_alias)

	if w_recn<>recno(w_alias) .or. .not. st_rlock(w_alias)
      do work17
      w_recn=recno(w_alias)
   endif
   if w_call
      unlock in (w_alias)
      clear read
   endif

else

	if w_recn<>recno(w_alias)
	   if &w_clave!=f17.d17defi+f17.d17orde and sino('Toma datos de otro rengl๓n')='S' 
   		private xd17defi,xd17orde
   		xd17defi=m.d17defi
	   	xd17orde=m.d17orde
		   =brow17()
	   	m.d17defi=xd17defi
		   m.d17orde=xd17orde
		   =seek(w_pref+&w_clave,w_alias) 
	   	w_recn=recno(w_alias)
	   else
			=work17(.t.)
  		   w_recn=recno(w_alias)
  		endif
   endif
   
endif

set orde to (w_orde) in (w_alias)
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC work17
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_eof

select (w_alias)
if !w_eof
	scatter memvar
else
	scatter memvar blank fields except d17defi,d17orde
   m.d17veco='V/C'

*===>
*  m.d17comp='FA/DB/CR/TI/RE/FC/RF/LV/TF/OT/RT'			&& R.02 Mariano
   m.d17comp='FA/DB/CR/TI/RE/FC/RF/LV/TF/TE/TD/OT/RT'	&& R.02 Mariano

   m.d17coim='1/2/3/4/5/6/7'
   m.d17copa='1/2/3'
   m.d17tipo='0- - - - - - - '
   m.d17prot='P/R'
   m.d17clas='1/2/3/A/B/C/D/E/F/G'
   m.d17dpro=space(3)
   m.d17dali=space(3)
   m.d17hpro='999'
   m.d17hali='999'
endif
if empty(m.d17prot)
	m.d17prot=' / '
endif
if empty(m.d17clas)
	m.d17clas=' / / / / / / / / / '

endif


if w_rdlevel<>rdlevel() and iif(version()='Visual Fox', !w_call,.t.)	&& ***vfp***
	=valid17(.t.)
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
PROC when17
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
w_retu=.t.
w_vari=varread()
p_curobj=_curobj
=irclave('D17ORDE','m.d17titu')

if !(w_vari='D17DEFI') and !(lastkey()=p_f4) and w_retu
	w_show=.t.
	do fhelp with 'D17DEFI','','iwp016','f16','d16defi','d16nomb','Definici๓n inexistente',(p_strc),'w_retu'
	if w_retu=.f.
		wait window 'Definici๓n inexistente'
		_curobj=objnum(m.d17defi)
	endif
	
endif

return(w_retu)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC valid17
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_show

***vfp***
if version()='Visual Fox' and (w_pfile!='17' or (!wexist('IWS017') and wontop()!='IWS017')) and w_show
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

if (w_vari='D17DEFI' and lastkey()=p_f4) or w_show
	do fhelp with 'D17DEFI','','iwp016','f16','d16defi','d16nomb','Definici๓n inexistente...',(p_strc),'w_retu'
endif

if w_vari='D17IMPR' or w_show
	if !m.d17impr$'SN'
		if !w_show
			wait wind 'Opciones vแlidas: S,N...'
		endif
		w_retu=.f.
	endif
endif

if w_vari='D17VECO' or w_show
	do case
	case !subs(m.d17veco,1,1)$'VC '
		if !w_show
			wait wind 'Error en Venta ๓ Compra.'
		endif
		w_retu=.f.
	case !subs(m.d17veco,3,1)$'VC '
		if !w_show
			wait wind 'Error en Venta ๓ Compra.'
		endif
		w_retu=.f.
	endcase
endif

if w_vari='D17COMP' or w_show
*===>
*	w__comp='FA.DB.CR.TI.RE.FC.RF.LV.TF.OT.RT  '	&& R.01 Mariano
*	w__comp='FA.DB.CR.TI.RE.FC.RF.LV.TF.TE.TD.OT.RT  '	&& R.01 Mariano	&& R.02 Mariano
	w__comp='FA.DB.CR.TI.RE.FC.RF.LV.TF.TC.TD.OT.RT  '	&& R.02 Mariano
	do case
	case !subs(m.d17comp,1,2)$w__comp
		if !w_show
	   	wait wind 'Error en Comprobantes.'
	   endif
	   w_retu=.f.
	case !subs(m.d17comp,4,2)$w__comp
		if !w_show
	   	wait wind 'Error en Comprobantes.'
	   endif
	   w_retu=.f.
	case !subs(m.d17comp,7,2)$w__comp
		if !w_show
	   	wait wind 'Error en Comprobantes.'
	   endif
	   w_retu=.f.
	case !subs(m.d17comp,10,2)$w__comp
		if !w_show
	   	wait wind 'Error en Comprobantes.'
	   endif
	   w_retu=.f.
	case !subs(m.d17comp,13,2)$w__comp
		if !w_show
	   	wait wind 'Error en Comprobantes.'
	   endif
	   w_retu=.f.
	case !subs(m.d17comp,16,2)$w__comp
		if !w_show
	   	wait wind 'Error en Comprobantes.'
	   endif
	   w_retu=.f.
	case !subs(m.d17comp,19,2)$w__comp
		if !w_show
	   	wait wind 'Error en Comprobantes.'
	   endif
	   w_retu=.f.
	case !subs(m.d17comp,22,2)$w__comp
		if !w_show
	   	wait wind 'Error en Comprobantes.'
	   endif
	   w_retu=.f.
	case !subs(m.d17comp,25,2)$w__comp
		if !w_show
	   	wait wind 'Error en Comprobantes.'
	   endif
	   w_retu=.f.
	case !subs(m.d17comp,28,2)$w__comp
		if !w_show
	   	wait wind 'Error en Comprobantes.'
	   endif
	   w_retu=.f.
*===>
	case !subs(m.d17comp,31,2)$w__comp
		if !w_show
	   	wait wind 'Error en Comprobantes.'
	   endif
	   w_retu=.f.
	&& R.01b Mariano
	case !subs(m.d17comp,34,2)$w__comp
		if !w_show
	   	wait wind 'Error en Comprobantes.'
	   endif
	   w_retu=.f.
	case !subs(m.d17comp,37,2)$w__comp
		if !w_show
	   	wait wind 'Error en Comprobantes.'
	   endif
	   w_retu=.f.
	&& R.01e Mariano
	endcase
endif

if w_vari='D17COIM' or w_show
	w_xcoim=m.d17coim
	m.d17coim=' '
	if lastkey()=p_f4
		do fhelp with 'D17COIM','TI','iwp001t','f01t','clav','desc','Tipo impositivo Inexistente',(p_char),'w_retu'
	endif
	m.d17coim=strtran(w_xcoim,' ',m.d17coim,1,1) && primer lugar vacio
	show get m.d17coim

   for wcasis=1 to 14 step 2
       if subs(m.d17coim,wcasis,1)<>space(1)
          if !seek('TI'+subs(m.d17coim,wcasis,1),'f01t')
          	 if !w_show
	             wait window 'Tipo impositivo: '+subs(m.d17coim,wcasis,1)+' Inexistente'
	          endif
             w_retu=.f.
             exit
          endif
       endif
   next
endif

if w_vari='D17COPA' or w_show
	w__copa='123 '
	do case
	case !subs(m.d17copa,1,1)$w__copa
		if !w_show
	   	wait wind 'Error en Condici๓n de pago...'
	   endif
	   w_retu=.f.
	case !subs(m.d17copa,3,1)$w__copa
		if !w_show
	   	wait wind 'Error en Condici๓n de pago...'
	   endif
	   w_retu=.f.
	case !subs(m.d17copa,5,1)$w__copa
		if !w_show
			wait wind 'Error en Condici๓n de pago...'
	   endif
	   w_retu=.f.
	endcase
endif

if w_vari='D17TIPO'
	w_xtipo=m.d17tipo
	m.d17tipo=' '
	if lastkey()=p_f4
		do fhelp with 'D17TIPO','GC','iwp021','f21','d21cla','d21des','',(p_char),'w_retu'
	endif
	m.d17tipo=strtran(w_xtipo,' ',m.d17tipo,1,1) && primer lugar vacio
	show get m.d17tipo

   for wcasis=1 to 16 step 2
       if subs(m.d17tipo,wcasis,1)<>spac(1)
          if !seek('GC'+subs(m.d17tipo,wcasis,1),'f21')
          	 if !w_show
	             wait window 'Grupo : '+subs(m.d17tipo,wcasis,1)+' Inexistente'
	          endif
             w_retu=.f.
             exit
          endif
       endif
   next
endif

if w_vari='D17PROT' or w_show
	w_xprot=m.d17prot
	m.d17prot=' '
	if lastkey()=p_f4
*===>
*		do fhelp with 'D17PROT','PR','iwp001t','f01t','clav','desc','',(p_char),'w_retu'
		do fhelp with 'D17PROT','PR','iwp021','f21','d21cla','d21des','Perc./Ret. inexistente',(p_char),'w_retu'

	endif
	m.d17prot=strtran(w_xprot,' ',m.d17prot,1,1) && primer lugar vacio
	show get m.d17prot

   for wcasis=1 to 4 step 2
       if subs(m.d17prot,wcasis,1)<>space(1)
*===>
*          if !seek('PR'+subs(m.d17prot,wcasis,1),'f01t')
          if !seek('PR'+subs(m.d17prot,wcasis,1),'f21')
          	 if !w_show
	             wait window 'Perc./Ret./Otros: '+subs(m.d17prot,wcasis,1)+' Inexistente'
	          endif
             w_retu=.f.
             exit
          endif
       endif
   next
endif

if w_vari='D17DPRO' or w_show
  	if ('TMC'$upper(m.d17form) or 'REN'$upper(m.d17form) or 'SRE'$upper(m.d17form))
  		m.d17dpro=space(3)
		show get m.d17dpro
  	else
		if lastk()=p_f4
			do fhelp with 'D17DPRO','','iwp024','f24','d24codi','d24codi','',(p_strc),'w_retu'
		else
*			m.d17dpro=strcero(m.d17dpro)
			if !seek(m.d17dpro,'f24') and !m.d17dpro='   '
				if !w_show
					wait window 'Perc./Ret. inexistente'		
				endif
				w_retu=.f.
			else
				show get m.d17dpro
			endif
		endif
	endif
endif

if w_vari='D17HPRO' or w_show
  	if ('TMC'$upper(m.d17form) or 'REN'$upper(m.d17form) or 'SRE'$upper(m.d17form))
  		m.d17hpro=space(3)
		show get m.d17hpro
  	else
		if lastk()=p_f4
			do fhelp with 'D17HPRO','','iwp024','f24','d24codi','d24codi','',(p_strc),'w_retu'
		else
			m.d17hpro=strcero(m.d17hpro)
			if !seek(m.d17hpro,'f24') and !m.d17hpro='999'
				if !w_show
					wait window 'Perc./Ret./Otros inexistente'		
				endif
				w_retu=.f.
			else
				show get m.d17hpro
			endif
		endif
	endif
	if m.d17dpro>m.d17hpro
		if !w_show
			wait wind 'Hasta debe ser mayor o igual que desde...'
		endif
		w_retu=.f.
	endif
endif

if w_vari='D17CLAS' or w_show
	w_xclas=m.d17clas
	m.d17clas=' '
	if lastkey()=p_f4
		do fhelp with 'D17CLAS','CL','iwp001t','f01t','clav','desc','',(p_char),'w_retu'
	endif
	m.d17clas=strtran(w_xclas,' ',m.d17clas,1,1) && primer lugar vacio
	show get m.d17clas

   for wcasis=1 to 20 step 2
       if subs(m.d17clas,wcasis,1)<>space(1)
          if !seek('CL'+subs(m.d17clas,wcasis,1),'f01t')
          	 if !w_show
	             wait window 'Clase de Oper.: '+subs(m.d17clas,wcasis,1)+' Inexistente'
	          endif
             w_retu=.f.
             exit
          endif
       endif
   next
endif

if w_vari='D17DALI' or w_show
  	if ('TMC'$upper(m.d17form) or 'REN'$upper(m.d17form) or 'SRE'$upper(m.d17form))
  		m.d17dali=space(3)
		show get m.d17dali
  	else
		if lastk()=p_f4
			do fhelp with 'D17DALI','','iwp023','f23','d23codi','d23codi','',(p_strc),'w_retu'
		else
			m.d17dali=strcero(m.d17dali)
			if !seek(m.d17dali,'f23') and !m.d17dali='000'
				if !w_show
					wait window 'Alicuota IVA inexistente'		
				endif
				w_retu=.f.
			else
				show get m.d17dali
			endif
		endif
	endif
endif

if w_vari='D17HALI' or w_show
  	if ('TMC'$upper(m.d17form) or 'REN'$upper(m.d17form) or 'SRE'$upper(m.d17form))
  		m.d17hali=space(3)
		show get m.d17hali
  	else
		if lastk()=p_f4
			do fhelp with 'D17HALI','','iwp023','f23','d23codi','d23codi','',(p_strc),'w_retu'
		else
			m.d17hali=strcero(m.d17hali)
			if !seek(m.d17hali,'f23') and !m.d17hali='999'
				if !w_show
					wait window 'Alicuota IVA inexistente'		
				endif
				w_retu=.f.
			else
				show get m.d17hali
			endif
		endif
	endif
	if m.d17dali>m.d17hali
		if !w_show
			wait wind 'Hasta debe ser mayor o igual que desde...'
		endif
		w_retu=.f.
	endif
endif

if w_vari='D17FILT' or w_show
	if !valifor(18,'m.d17filt')
		w_retu=.f.
	endif
endif

if w_vari='D17FORM' or w_show
   if valifor(20,'m.d17form')
   	if ('TMC'$upper(m.d17form) or 'REN'$upper(m.d17form) or 'SRE'$upper(m.d17form))
   		*)Si existen estas funciones no le da bola 
         *)a los filtros fijos         
         m.d17veco=' / '
         m.d17comp='  /  /  /  /  /  /  /  /  /  /  '	&& R.02 Mariano
 		 m.d17comp='  /  /  /  /  /  /  /  /  /  /  /  /  '	&& R.02 Mariano
         m.d17coim=' / / / / / / '
         m.d17copa=' / / '
         m.d17tipo=' - - - - - - - '
         m.d17filt=''
		   m.d17prot=' / / '
		   m.d17clas=' / / / / / / / / / '
		   m.d17dpro=space(3)
		   m.d17hpro=space(3)
		   m.d17dali=space(3)
		   m.d17hali=space(3)
         show gets
      endif
   else
   	w_retu=.f.
	endif
endif

=ultdepri('D17DEFI','m.d17come')
=prideult('D17COME','m.d17come')
=modicampo('D17DEFI','D17ORDE')
return(w_retu)

***vfp***
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc actu17
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	on key label f9 do actu17x
	keyboard '{f9}'
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc actu17x
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

if !valid17(.t.)
   wait 'Debe completar datos...' wind
	return
endif

do clav17
if .not. eof(w_alias) .and. .not. st_rlock(w_alias)
   return
endif

=db_capt(w_alias)
w_orde=order(w_alias)
set orde to 'd17defi' in (w_alias)

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
      do work17
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
PROC brow17
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
push menu _msysmenu
=skipbar('Registro','Insertar' ,!w_call)
=skipbar('Registro','Agregar'	 ,.t.)
=skipbar('Registro','Eliminar' ,.t.)
=skipbar('Registro','Modificar',.t.)
=skipbar('Registro','Consultar',.t.)
=skipbar('Registro','Elegir'	 ,.t.)

w_orde=order(w_alias)
set order to tag 'd17titu' in (w_alias)
if eof(w_alias)
	go top in (w_alias)
endif

sele (w_alias)
browse fields;
	d17defi :h='Def'	 :p='@K':v=fenter():f,;
	d17orde :h='Nro'	 :p='@K',;
	d17titu :h='Tํtulo':p='@K';
	nome noap noed node norm in screen title (w_title) font 'arial',10;
	valid :f iif(w_call,.t.,salebrow(Lastkey()))

if !w_call
	=work17()
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
PROC busc17
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
w_orde=order(w_alias)
set order to tag 'd17titu' in (w_alias)
do busca
if .not. eof(w_alias)
	do work17
endif
set orde to (w_orde) in (w_alias)
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC orde17
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
do case 
   case prompt()='Ordenado por C๓digo'
      set orde to 'd17defi' in (w_alias)
   case prompt()='Ordenado por Descripci๓n'
     	set orde to 'd17titu' in (w_alias)
endcase
if wexist(w_title)
	show wind (w_title) refresh
endif
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC ValiFOR
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_colu,w_name

PRIVATE w_retu
w_retu=.t.

w_form=&w_name
if !empty(w_form)
	w_mens=ValiForm(w_form)
	
	on error do erro17
   w_tform=&w_form
   on error do ssp090 with error(),lineno(),program()
   if !w_retu
   	return(.f.)
   endif
   
   w_type=type('w_tform')
   do case
      case !empty(w_mens)
         =advsuave(w_mens+' - Funci๓n Desconocida...')
         w_retu=.f.
      case !w_type$'L' .and. w_name='m.d17filt'
         =advsuave('Error en Filtro...')
         w_retu=.f.
      case !(w_type=='N'.or.w_type=='UI') .and. w_name='D17FORM'
         =advsuave('Error en Cแlculo...')
         w_retu=.f.
      otherwise
   endcase
endif
return(w_retu)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC help17
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
push menu _msysmenu
set skip of menu _msysmenu .t.

PRIVATE w_vari
w_vari=varread()

if w_vari='D17FORM'
	set status bar off
	DEFINE WINDOW pantalla1 ;
	AT  0.000, 0.000  ;
	SIZE 20.667,76.500 ;
	FONT 'Letter Gothic',12 ;
	TITLE "Funci๓n"+space(40)+'<Esc> -> para salir' ;
	SYSTEM 
	
	zoom wind pantalla1 max

	DEFINE WINDOW pantalla2 ;
		AT  0.000, 0.000  ;
		SIZE 17.667,66.500 ;
		FONT 'Letter Gothic',12 ;
		system ;
		in window pantalla1

	zoom wind pantalla2 max

	acti window pantalla1

	modi file doc\funcion.txt window pantalla2 noedit nomenu

	rele wind pantalla1,pantalla2

	set status bar on

endif

pop menu _msysmenu
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC reng17
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
push menu _msysmenu
set skip of menu _msysmenu .t.

w_orde=order(w_alias)
set order to tag 'd17defi' in (w_alias)
if eof(w_alias)
	go top in (w_alias)
endif

sele (w_alias)
browse fields;
	d17orde :h='Nro'	 :p='@K',;
	d17titu :h='Tํtulo':p='@K';
	nome noap noed node norm in screen title (w_title) font 'arial',10;
	key d17defi
	
set order to tag (w_orde) in (w_alias)
pop menu _msysmenu
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC func17
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PRIVATE w_vari,w_stat,w_titu
w_vari=VARREAD()
w_stat=.f.

if !w_vari$'D17FILT.D17FORM'
	return
endif

push menu _msysmenu
set skip of menu _msysmenu .t.

w_titu=db_name('F18')

sele f18
if eof()
	go top
endif
browse fields;
	field_des :r:h='Dato'  :p='@K':v=fenter17():f,;
	field_name:r:h='Nombre':p='@K',;
	field_type:r:h='T'	  :p='@K',;
	field_len :r:h='L'	  :p='@KZ',;
	field_dec :r:h='D'	  :p='@KZ';
	nome noap noed node norm in screen title w_titu font 'arial',10
	
if w_stat
	do case
	case w_vari='D17FILT'
		m.d17filt=allt(m.d17filt)+f18.field_name
		show get m.d17filt
	case w_vari='D17FORM'
		m.d17form=allt(m.d17form)+f18.field_name
		show get m.d17form
	endcase
endif

pop menu _msysmenu
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC fenter17
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
if lastk()=13
	w_stat=.t.
	keyb '{ctrl+w}'
endif
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC erro17
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
=advsuave('ERROR de f๓rmula o filtro...')
w_retu=.f.
return
