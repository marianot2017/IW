*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP004.PRG -> Mantenimiento de periodos de IVA
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

if wexist('iws005r') or wexist('iws005p') or wexist('iws005j')
	do case
		case (w_vta and w_cpra) or (!w_vta and !w_cpra)
			xopci='VC'
		case w_vta
			xopci='V'
		case w_cpra
			xopci='C'
	endcase
endif

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

w_imco=imco()

*) Datos del archivo a mantener------------------------------------------------

w_pfile='04'
w_clave='ptoi(m.d4peri)'
w_alias='F'+w_pfile
w_title=db_name(w_alias)

store 0 to w_tot1,w_tot2,w_tot3,wasaf1,wasaf2
store 0 to w_tcv,w_tcv1,w_tcv2,w_tcv3,w_tcv4,w_tcv5,w_tcv6,w_tcv7,w_tcv8

*) Apertura de archivos--------------------------------------------------------

if .not. w_call

	fieldvec[1]='F00'
	fieldvec[2]='F04'
	fieldvec[3]='F05'
	fieldvec[4]='F13'

	use (d0f00) in 0 alias 'f00'						     &&(Tablas)
	use (d0f04) in 0 alias 'f04' order 'd4peri' exclu &&(Periodos)
	use (d0f05) in 0 alias 'f05' order 'd5peri'		  &&(Comprobantes)
	use (d0f13) in 0 alias 'f13' order 'd13peri'		  &&(Anticipos y retenciones)
	
	if neterr()
		pop key
		pop menu _msysmenu
		fieldvec=''
		=closall()
		return
	endif
	
	fieldvec=''
endif


*)-----------------------------------------------------------------------------

select (w_alias)
GO BOTTOM
SCATTER MEMVAR
=valid04(.T.)
*scatter memvar blank

set skip of menu _msysmenu .t.
do iwm004.mpr &&(menu registro extendido)

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
*	do brow04
    if version()='Visual FoxPro'
		*** cambio el lastkey() ***
		keyboard '{tab}'
		=inkey()
		DO wbrow WITH 'brow04()'
    else
   		do brow04
    endif
    ***vfp***
   if w_alta
      w_alta=.f.
      scatter memvar blank
      do iws004.spr
   endif

else 

	***vfp***
    if version()='Visual FoxPro'
	    on key label esc do actsalir
	endif
	***vfp***
	do iws004.spr
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
   set orde to 'd4peri' in (w_alias)
   if .not. empty(w_alan)
   	sele (w_alan)
   	w_alias=w_alan
   endif
   rele wind iws004
else
	rele wind iws004,barraabm
	=closall()
endif
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC Alta04
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
if w_call
	w_alta=.t.
   deac wind (w_title)
endif
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC Baja04
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
if seek(PtoI(m.d4peri),'f05')
	=advsuave('Perํodo de IVA con Comprobantes.')
	return
endif
if seek(PtoI(m.d4peri),'f13')
	=advsuave('Existen Anticipos y Retenciones para el Perํodo.')
	return
endif

do clav04
if eof(w_alias) .or. .not. st_rlock(w_alias)
   return 
endif

if sino('Acepta eliminaci๓n ?.')='S'
	if begintran()
		=dele_rec(w_alias)
		unlock in (w_alias)
		=audi('BPE',m.d4peri,'')
     	=endtran()
	endif
	do siguiente
endif
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC clav04
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
m.d4peri=strcero(left(m.d4peri,2))+'/'+strcero(right(m.d4peri,4))
show get m.d4peri

if w_recn<>0
   w_recn=recno(w_alias)
endif

w_orde=order(w_alias)
set orde to 'd4peri' in (w_alias)

if seek(w_pref+&w_clave,w_alias)

   if w_recn<>recno(w_alias) .or. .not. st_rlock(w_alias)
      do work04
      w_recn=recno(w_alias)
   endif
   if w_call
      unlock in (w_alias)
      clear read
   endif

else

	if w_recn<>recno(w_alias)
		w_peri=m.d4peri
      do work04      
      m.d4peri=w_peri
      m.d4coim=d0coim
      w_recn=recno(w_alias)
   	show get m.d4peri
   endif
   
endif

set orde to (w_orde) in (w_alias)
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC work04
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
select (w_alias)
scatter memvar

if w_rdlevel<>rdlevel() and iif(version()='Visual Fox', !w_call,.t.)	&& ***vfp***
	=valid04(.t.)	
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
PROC when04
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
w_retu=.t.
w_vari=varread()
p_curobj=_curobj

if w_imco
	@ 4.933,25.125 TO 6.066,50.875 ;
		PEN 1, 8
	@ 5.000,25.375 say m.d4asve ;
		SIZE 1.000,25.125 ;
		FONT "Fixedsys", 9 ;
		PICTURE "@K" ;
		COLOR RGB(0,0,255,,,,)
	@ 6.200,25.125 TO 7.333,50.875 ;
		PEN 1, 8
	@ 6.267,25.375 say m.d4asco ;
		SIZE 1.000,25.125 ;
		FONT "Fixedsys", 9 ;
		PICTURE "@K" ;
		COLOR RGB(0,0,255,,,,)
endif

*if !eof('f04')	&& No es alta
*	if w_vari='D4COIM'
*		=valid04(.f.)
*		w_retu=.f.
*	endif
	=irclave('D4PERI','m.d4esco')
*else
*	=irclave('D4PERI','m.d4coim')
*endif

return(w_retu)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC valid04
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_show

***vfp***
if version()='Visual Fox' and (w_pfile!='04' or (!wexist('IWS004') and wontop()!='IWS004')) and w_show
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

*???if w_show
	w_tcv =m.d4vtot-m.d4ctot
	w_tcv1=m.d4vim1-m.d4cim1
	w_tcv2=m.d4vim2-m.d4cim2
	w_tcv3=m.d4vim3-m.d4cim3
	w_tcv4=m.d4vim4-m.d4cim4
	w_tcv5=m.d4vim5-m.d4cim5
	w_tcv6=m.d4vim6-m.d4cim6
	w_tcv7=m.d4vim7-m.d4cim7
	w_tcv8=m.d4vim8-m.d4cim8

	wrec=0
	if !bof('f04') and !eof('f04')
		wrec=recno('f04')
		skip -1 in f04		
	endif
	wasaf1=iif(bof('f04'),0,f04.d4saf1)
	wasaf2=-wasaf1
	if !empty(wrec)
		go wrec in f04
	endif
	
*	w_tot1 = m.d4cim4 + m.d4cim5 + m.d4cim3 + m.d4cim8 + wasaf1
	w_tot1 = m.d4cim4 + m.d4cim5 + m.d4cim3 + wasaf1

*	w_tot2 = m.d4vim4 + m.d4vim5 + m.d4vim3 + m.d4vim8
	w_tot2 = m.d4vim4 + m.d4vim5 + m.d4vim3

	w_tot3 = w_tot2 - w_tot1
*???endif

IF EMPTY(m.d4esve)
			m.d4esve	= "A"
ENDIF			

IF EMPTY(m.d4esco)
			m.d4esco	= "A"
ENDIF			

if w_vari='D4PERI' or w_show
	w_fech	= CTOD('01/' + m.d4peri)
	IF EMPTY(w_fech)
					RETURN	.F. 
	ENDIF				
	IF TRDATE('w_fech')
			m.d4peri	= RIGHT(DTOC(w_fech),7)
			SHOW GET m.d4peri
	ELSE
		IF !w_show
				WAIT WIND "El perํodo no es correcto"
		ENDIF
		w_retu	= .F.
		_curobj	= objnum(m.d4peri)
	ENDIF
endif

if w_vari='D4COIM' or w_show
	if !m.d4coim$'123'
		if !w_show
			wait wind 'Opciones vแlidas: 1,2,3...'
		endif
		w_retu=.f.
	endif
endif

if w_vari='D4ESVE' or w_show
	if !m.d4esve$'AC'
		if !w_show
			wait wind 'Opciones vแlidas: A,C...'
		endif
		w_retu=.f.
	endif
endif

if w_vari='D4ESCO' or w_show
	if !m.d4esco$'AC'
		if !w_show
			wait wind 'Opciones vแlidas: A,C...'
		endif
		w_retu=.f.
	endif
endif

*show gets	&& R.01 Mariano

=ultdepri('D4PERI','m.d4saf1')
=prideult('D4SAF1','m.d4saf1')
=modicampo('D4PERI')
return(w_retu)

***vfp***
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc actu04
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	on key label f9 do actu04x
	keyboard '{f9}'
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc actu04x
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

if !valid04(.t.)
   wait 'Debe completar datos...' wind
	return
endif

do clav04
if .not. eof(w_alias) .and. .not. st_rlock(w_alias)
   return 
endif

=db_capt(w_alias)
w_orde=order(w_alias)
set orde to 'd4peri' in (w_alias)

if .not. seek(w_pref+&w_clave,w_alias)

	if w_agre
      wait 'Agregando registro...' wind time .4
		if begintran()
	      =net_appe(w_alias)
	      sele (w_alias)
   	   gather memvar 
   	   =vactu04()
	   =audi('APE',m.d4peri,'')
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
				=vactu04()
			   =audi('MPE',m.d4peri+' '+iif(m.d4esve='A','Abre','Cierre'),m.d4peri+' '+iif(m.d4esco='A','Abre','Cierre'))
   	      =endtran()
   	   endif
         w_modicampo=.f.
      endif 
   else 
      do work04
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
PROC vactu04
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
w_rec=recno('f04')
go bott in 'f04'

if w_rec=recno('f04') .and. d0coim<>f04->d4coim
	=db_flock('f00')
	sele f00
	repl p0coim with f04->d4coim	
	go (recno('f00')) in f00
	d0coim=f00->p0coim
   unlock in f00
endif

go (w_rec) in f04
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC brow04
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
if eof(w_alias)
	go top in (w_alias)
endif

sele (w_alias)

do case
	case wexist('iws005v') or wexist('iws005e') or wexist('iws015') or ;
			(wexist('iws005r') and xopci$'V') or (wexist('iws005p') and xopci$'V') or ;
			(wexist('iws005j') and xopci$'V') or wexist('iws013')
		browse fields ;
			d4peri :h='Perํodo':p='@K':v=fenter():f,;
			estado=iif(d4esve='A','Abierto','Cerrado') :r;
			nome noap noed node norm in screen title (w_title) font 'arial',10;
			valid :f iif(w_call,.t.,salebrow(Lastkey()))
	case wexist('iws005c') or (wexist('iws005r') and xopci$'C') or ;
			(wexist('iws005p') and xopci$'C') or (wexist('iws005j') and xopci$'C') ;
			or wexist('iws005o')
		browse fields ;
			d4peri :h='Perํodo':p='@K':v=fenter():f,;
			estado=iif(d4esco='A','Abierto','Cerrado') :r;
			nome noap noed node norm in screen title (w_title) font 'arial',10;
			valid :f iif(w_call,.t.,salebrow(Lastkey()))
	case wexist('iws005i') or wexist('iws005t')
		browse fields ;
		d4peri :h='Perํodo':p='@K':v=fenter():f,;
		esve=iif(d4esve='A','Abierto','Cerrado') :r :h='Estado Ventas',;
		esco=iif(d4esco='A','Abierto','Cerrado') :r :h='Estado Compras';
		nome noap noed node norm in screen title (w_title) font 'arial',10;
		valid :f iif(w_call,.t.,salebrow(Lastkey()))
	case wexist('iws017p')
		browse fields ;
		d4peri :h='Perํodo':p='@K':v=fenter():f,;
		esve=iif(d4esve='A','Abierto','Cerrado') :r :h='Estado Ventas',;
		esco=iif(d4esco='A','Abierto','Cerrado') :r :h='Estado Compras',;
		d4vtot :h='Total Ventas' :r, d4ctot :h='Total Compras' :r ;
		nome noap noed node norm in screen title (w_title) font 'arial',10;
		valid :f iif(w_call,.t.,salebrow(Lastkey()))
	otherwise
		browse fields ;
		d4peri :h='Perํodo':p='@K':v=fenter():f,;
		esve=iif(d4esve='A','Abierto','Cerrado') :r :h='Estado Ventas',;
		esco=iif(d4esco='A','Abierto','Cerrado') :r :h='Estado Compras',;
		d4hove :h='Ult.Hoj.Vtas.',;
		d4hoco :h='Ult.Hoj.Cpras.';
		nome noap noed node norm in screen title (w_title) font 'arial',10;
		valid :f iif(w_call,.t.,salebrow(Lastkey()))
endcase

if !w_call
	=work04()
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
PROC busc04
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
w_orde=order(w_alias)
do busca
if .not. eof(w_alias)
	do work04
endif
set orde to (w_orde) in (w_alias)
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC orde04
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
do case 
	case prompt()='Ordenado por C๓digo'
    	set orde to 'd4peri' in (w_alias)
	case prompt()='Ordenado por Descripci๓n'
		set orde to 'd4nomb' in (w_alias)
endcase
if wexist(w_title)
	show wind (w_title) refresh
endif
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC impe04
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
if allt(m.d4peri)=='/'
	=advsuave('No se registran datos para este perํodo. Debe especificar uno...')
	return
endif
if sino('Confirma la impresi๓n de este perํodo ?.','N')=='S'
	push menu _msysmenu &&(Salva estado de menu)
	set skip of menu _msysmenu .t.
	on key label f9
	do ssm998.mpr
	do getdest.spr
	if w_agre
		on key label f9 keyboard '{ctrl+f9}'
	endif
	pop menu _msysmenu	&&(restaura menu anterior)
endif

return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC Cons04
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
if .not. valid04(.t.)
	wait wind 'Verifique los datos ingresados...'
	return
endif

w_recn=recno(w_alias)

sele (w_alias)	&& R.03 Mariano 

do repdest &&(Setea variables internas del reporte)

if !empty(p_repclaus) &&(debe ser preview o to printer)
	p_report='iwr004i'
	=rep_form()
	p_report=''
endif

do represet &&(Resetea estado)

GO w_recn
return


***puesto adrede para que incluya el report al proyecto**
report form iwr004i
