*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP020.PRG -> Tipos Impositivos por cliente y periodo
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
PARAMETERS w_pref,w_call,w_pclpr

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

w_pfile='20'
w_clave='w_pclpr+m.d20clie+PtoI(m.d20peri)'
w_alias='F'+w_pfile
w_title=db_name(w_alias)
w_imco=imco()

*) Apertura de archivos--------------------------------------------------------


if !w_call

	fieldvec[1]='F00'
	fieldvec[2]='F01'
	fieldvec[3]='F02'
	fieldvec[4]='F03'
	fieldvec[5]='F04'
	fieldvec[6]='F20'

	use (d0f00) in 0 alias 'f00'						&&(Parametros)
	use (d0f01) in 0 alias 'f01' order 'd1tip'   &&(Tablas)
	use (d0f02) in 0 alias 'f02' order 'd2clie'  &&(Clientes)
	use (d0f03) in 0 alias 'f03' order 'd3prov'  &&(Proveedores)
	use (d0f04) in 0 alias 'f04' order 'd4peri'  &&(Periodos)
	use (d0f20) in 0 alias 'f20' order 'd20tipo' &&(Tipos impos. x periodos)

	if w_imco		
		ncf01=w0fipre+'01'
		fieldvec[7]='F01c'
		use (ncf01) in 0 alias f01c order tag 'd1cuen'
	endif
	
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
	
	=creatipos(w_pclpr)
	
endif

fieldvec=''

*)-----------------------------------------------------------------------------

if w_pclpr='C'
   w_tiimva='1234567'
   w_clpr='Cliente'
else
   w_tiimva='12456'
   w_clpr='Proveedor'
endif

sele f20
scatter memvar blank
set filt to d20tipo=w_pclpr
go top

set skip of menu _msysmenu .t.
do ssm999.mpr	&&(menu registro)

=skipbar('registro','insertar',.t.)
=skipbar('registro','Ordenado por C๓digo',.t.)
=skipbar('registro','Ordenado por Descripci๓n',.t.)
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
*	do brow20
    if version()='Visual FoxPro'
		*** cambio el lastkey() ***
		keyboard '{tab}'
		=inkey()
		DO wbrow WITH 'brow20()'
    else
   		do brow20
    endif
    ***vfp***
	if w_alta
      w_alta=.f.
      do iws020.spr
   endif

else 

	***vfp***
    if version()='Visual FoxPro'
	    on key label esc do actsalir
	endif
	***vfp***
	do iws020.spr
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
   set orde to 'd20tipo' in (w_alias)
   if .not. empty(w_alan)
   	sele (w_alan)
   	w_alias=w_alan
   endif
   rele wind iws020
else
	rele wind iws020,barraabm
	=closall()
	=borratipos()
endif
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC Alta20
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
if w_call
	w_alta=.t.
   deac wind (w_title)
endif
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC Baja20
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
do clav20
if eof(w_alias) .or. .not. st_rlock(w_alias)
   return 
endif

if sino('Acepta eliminaci๓n ?.')='S'
	if begintran()
		sele (w_alias)
		=dele_rec(w_alias)
      unlock in (w_alias)
  	   =vactu20()
      =endtran()
	endif
	do siguiente
endif
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC clav20
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
if w_recn<>0
   w_recn=recno(w_alias)
endif

w_orde=order(w_alias)
set orde to 'd20tipo' in (w_alias)

if seek(w_pref+&w_clave,w_alias)

   if w_recn<>recno(w_alias) .or. .not. st_rlock(w_alias)
      do work20
      w_recn=recno(w_alias)
   endif
   if w_call
      unlock in (w_alias)
      clear read
   endif

else

	if w_recn<>recno(w_alias)
      =work20(.t.)
      w_recn=recno(w_alias)
   endif
   
endif

set orde to (w_orde) in (w_alias)
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC work20
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_eof

select (w_alias)
if !w_eof
	scatter memvar
else
	scatter memvar blank fields except d20tipo,d20clie,d20peri
endif
m.d20tipo=w_pclpr

if w_rdlevel<>rdlevel() and iif(version()='Visual Fox', !w_call,.t.)	&& ***vfp***
	=valid20(.t.)
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
PROC when20
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
w_retu=.t.
w_vari=varread()
p_curobj=_curobj
=irclave('D20PERI','m.d20tiim')

if !(w_vari='D20CLIE') and !(lastkey()=p_f4) and w_retu		&& valido aca porque es primer campo
	if w_pclpr='C'
		w_show=.t.	&& debe ser true para que evalue la fhelp
		do fhelp with 'D20CLIE','','iwp002','f02','d2clie','d2nomb','Cliente inexistente',(p_strc),'w_retu'
	else
		w_show=.t.	&& debe ser true para que evalue la fhelp
		do fhelp with 'D20CLIE','','iwp003','f03','d3prov','d3nomb','Proveedor inexistente',(p_strc),'w_retu'
	endif
	if w_retu=.f.
		if w_pclpr='C'
			wait window 'Cliente inexistente'
		else
			wait window 'Proveedor inexistente'
		endif
		_curobj=objnum(m.d20clie)
	endif
endif

return(w_retu)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC valid20
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_show

***vfp***
if version()='Visual Fox' and (w_pfile!='20' or (!wexist('IWS020') and wontop()!='IWS020')) and w_show
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

*===>
w_sosretu	= .T.		&&para almacenar el w_retu antes de usar la FHELP xa validar

w_vari=varread()
w_vread='m.'+w_vari

if (w_vari='D20CLIE' and lastkey()=p_f4) or w_show
	if w_pclpr='C'
		do fhelp with 'D20CLIE','','iwp002','f02','d2clie','d2nomb','Cliente inexistente',(p_strc),'w_retu'
	else
		do fhelp with 'D20CLIE','','iwp003','f03','d3prov','d3nomb','Proveedor inexistente',(p_strc),'w_retu'
	endif
endif

IF w_vari	= 'D20PERI' .OR. w_show
		w_fech	= CTOD('01/' + m.d20peri)
		IF EMPTY(w_fech)
						RETURN	.F. 
		ENDIF
		IF TRDATE('w_fech')
				m.d20peri	= RIGHT(DTOC(w_fech),7)
				SHOW GET m.d20peri
		ELSE
			IF !w_show
					WAIT WIND "El perํodo no es correcto"
			ENDIF
			w_retu	= .F.
			_curobj	= objnum(m.d20peri)
			RETURN .F.
		ENDIF
ENDIF
*===>


if w_vari='D20TIIM' or w_show

	w_sosretu	= w_retu			&& *===> Salvo el w_retu
	do fhelp with 'D20TIIM','TI','iwp001t','f01t','clav','desc','Tipo impositivo Inexistente',(p_char),'w_retu'

*===> Agregado para que se posicione en el tipo si es el ๚nico campo con error
	IF w_sosretu	.AND. !w_retu
				_curobj = 3
	ENDIF				
*===>

endif


=ultdepri('D20CLIE','m.d20tiim')
=prideult('D20TIIM','m.d20tiim')
=modicampo('D20TIPO','D20CLIE','D20PERI')

*===> Puede ser que la ๚ltima validaci๓n est้ OK y hubo alguna inconsistencia antes
IF !w_sosretu	
		w_retu	= .F.
ENDIF
*===>		

return(w_retu)

***vfp***
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc actu20
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	on key label f9 do actu20x
	keyboard '{f9}'
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc actu20x
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

if !valid20(.t.)
   wait 'Debe completar datos...' wind
	return
endif

do clav20
if .not. eof(w_alias) .and. .not. st_rlock(w_alias)
   return 
endif

=db_capt(w_alias)
w_orde=order(w_alias)
set orde to 'd20tipo' in (w_alias)

if !seek(w_pref+&w_clave,w_alias)

	if w_agre
      wait 'Agregando registro...' wind time .4
		if begintran()
	      =net_appe(w_alias)
	      sele (w_alias)	      
   	   gather memvar 
   	   =vactu20()
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
				=vactu20()
   	      =endtran()
   	   endif
         w_modicampo=.f.
      endif 
   else 
      do work20
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
PROC vactu20
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
w_rec=iif(!eof('f20'),recno('f20'),0)

* Busco el ultimo registro para ese cliente
*=start('f20',m.d20tipo+m.d20clie+'9999999')
*skip -1 in f20

=seek(m.d20tipo+m.d20clie,'f20')
sele f20
scan while f20.d20tipo=m.d20tipo and f20.d20clie=m.d20clie
endscan
skip -1 in f20

*altd()

if f20.d20tipo=m.d20tipo and f20.d20clie=m.d20clie
	if m.d20tipo='C'
		=seek(m.d20clie,'f02')
		=db_rlock('f02')
	  	sele f02
	  	repl f02->d2tipo with f20->d20tiim
	  	unlock in 'f02'
	else
		=seek(m.d20clie,'f03')
		=db_rlock('f03')
		sele f03
	   repl f03->d3tipo with f20->d20tiim
	   unlock in 'f03'
	endif
endif

if !empty(w_rec)
	go (w_rec) in f20
endif
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC brow20
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
push menu _msysmenu
=skipbar('Registro','Insertar' ,!w_call)
=skipbar('Registro','Agregar'	 ,.t.)
=skipbar('Registro','Eliminar' ,.t.)
=skipbar('Registro','Modificar',.t.)
=skipbar('Registro','Consultar',.t.)
=skipbar('Registro','Elegir'	 ,.t.)

w_orde=order(w_alias)
*set orde to 'd2nomb' in (w_alias)
if eof(w_alias)
	go top in (w_alias)
endif

sele (w_alias)
browse fields ;
	d20clie :h=(w_clpr) :p='@K':v=fenter():f,;
	d20peri :h='Perํodo':p='@K',;
	d20tiim :h='T.Impositivo';
	nome noap noed node norm in screen title (w_title) font 'arial',10;
	valid :f iif(w_call,.t.,salebrow(Lastkey()))

if !w_call
	=work20()
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
PROC busc20
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
w_orde=order(w_alias)
do busca
if .not. eof(w_alias)
	do work20
endif
set orde to (w_orde) in (w_alias)
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC orde20
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
do case 
   case prompt()='Ordenado por C๓digo'
      set orde to 'd2clie' in (w_alias)
   case prompt()='Ordenado por Descripci๓n'
     	set orde to 'd2nomb' in (w_alias)
endcase
if wexist(w_title)
	show wind (w_title) refresh
endif
return
