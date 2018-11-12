*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP001.PRG -> Mantenimiento de Tablas
*)-H Release		 : 04
*)-H Fecha de inicio : 22/12/1998.-
*)-H Fecha de fin    : 29/12/1998.-
*)-H
*)-H Actualizaciones
*)-H
*)-H Fecha       Programmer      Comentarios
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
parameters w_pref,w_call

private all like w_*
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

w_pfile='01'
w_clave='m.d1cla'
w_alias='F'+w_pfile

w_title=ftable(w_pref,'title')

w_imco=imco()
w_pepe	= 0

if w_pref$'RV.RC.RP.RS'
	x_ultget=iif(w_imco,'D1CIMP8','D1ALIC')		&& R.01 Mariano (w_ultget por x_ultget)
else
	x_ultget='D1TDGI'	&& R.01 Mariano (w_ultget por x_ultget)
endif

w_pcimp		= " "
w_pcimp1	= " "
w_pcimp2	= " "
w_pcimp3	= " "
pn_n	= 4
pn_m	= 4
pn_l	= 4

IF w_pref = 'JU'
			x_ultget='D1TDGI'	&& R.01 Mariano (w_ultget por x_ultget)
ENDIF

IF !w_imco	AND 	w_pref <>  'JU'
			IF w_pref $ 'RV.RC'
						x_ultget	= 'D1ALIC'	&& R.01 Mariano (w_ultget por x_ultget)
			ELSE
				x_ultget	= 'D1CLAS'	&& R.01 Mariano (w_ultget por x_ultget)
			ENDIF
ENDIF



*) Apertura de archivos--------------------------------------------------------


if !w_call

	fieldvec[1]='F00'
	fieldvec[2]='F01'
	fieldvec[3]='F13'
	fieldvec[4]='F23'

	use (d0f00) in 0 alias 'f00'						&&(Parametros)
	use (d0f01) in 0 alias 'f01' order 'd1tip'   &&(Tablas)
	use (d0f13) in 0 alias 'f13' order 'd13peri' &&(Anticipos y retenciones)
	use (d0f23) in 0 alias 'f23' order 'd23codi' &&(Alicuotas IVA)

	if w_imco
		ncf01=w0fipre+'01'

		fieldvec[5]='F01C'
		use (ncf01) in 0 alias 'f01c' order 'd1cuen'

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

	=creatipos('V')	&& clase de operacion

endif

fieldvec=''

*)-----------------------------------------------------------------------------
*?????  Pinchaba cdo se llama dde browse de rubro en ALTA DE COMPROBANTE
*if !w_call
	go top in f00
	if w_pref$'RV.RS'
		w_tocv1=f00.p0ocv1
		w_tocv2=f00.p0ocv2
		w_tocv3=f00.p0ocv3
	else
		w_tocv1=f00.p0occ1
		w_tocv2=f00.p0occ2
		w_tocv3=f00.p0occ3
	endif
*endif	

select (w_alias)
if !w_call
	set filter to d1tip$w_pref
	go top
	scatter memvar blank
	m.d1tip=w_pref
endif

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
*	do brow01
    if version()='Visual FoxPro'
		*** cambio el lastkey() ***
		keyboard '{tab}'
		=inkey()
		DO wbrow WITH 'brow01()'
    else
   		do brow01
    endif
    ***vfp***
	if w_alta
		w_alta=.f.
		scatter memvar blank
		do case
			case w_pref$'RV.RS'
				* Por Alta ultimo numero
            =start(w_pref+'Z    ','f01')
            if !bof('f01')
               skip -1 in f01
               if f01->d1tip=w_pref
                  m.d1cla=strcero(str(val(f01->d1cla)+1,6))
               endif   
			endif
				*
				if w_imco
						IF w_pref = 'RV'
									do iws001.spr
						ELSE
							do iws001r.spr
						ENDIF					
				else
					IF w_pref = 'RV'
								do iws001b.spr
					ELSE
						do iws001rc.spr
					ENDIF					
				endif
			case w_pref$'RC.RP'
				* Por Alta ultimo numero
            =start(w_pref+'Z    ','f01')
            if !bof('f01')
	            skip -1 in f01
               if f01->d1tip=w_pref
                  m.d1cla=strcero(str(val(f01->d1cla)+1,6))
               endif   
			endif
				*
				if w_imco
						IF w_pref = 'RC'
									do iws001c.spr
						ELSE
							do iws001r.spr
						ENDIF					
				else
					IF w_pref = 'RC'
								do iws001bc.spr
					ELSE
						do iws001rc.spr
					ENDIF					
				endif
			otherwise
				do iws001j.spr
		endcase
	endif
else

	***vfp***
    if version()='Visual FoxPro'
	    on key label esc do actsalir
	endif
	***vfp***
	do case
		case w_pref$'RV.RS'
				if w_imco
						IF w_pref = 'RV'
									do iws001.spr
						ELSE
							do iws001r.spr
						ENDIF					
				else
					IF w_pref = 'RV'
								do iws001b.spr
					ELSE
						do iws001rc.spr
					ENDIF					
				endif
		case w_pref$'RC.RP'
				if w_imco
						IF w_pref = 'RC'
									do iws001c.spr
						ELSE
							do iws001r.spr
						ENDIF					
				else
					IF w_pref = 'RC'
								do iws001bc.spr
					ELSE
						do iws001rc.spr
					ENDIF					
				endif
		otherwise
			do iws001j.spr
	endcase
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
	set orde to 'd1tip' in (w_alias)
	if .not. empty(w_alan)
		sele (w_alan)
		w_alias=w_alan
	endif
	rele wind iws001,iws001b,iws001c,iws001bc,iws001j
else
	rele wind iws001,iws001b,iws001c,iws001bc,iws001j,barraabm
	=closall()
	=borratipos()
endif
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc alta01
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
if w_call
	w_alta=.t.
	deac wind (w_title)
endif
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc baja01
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
if w_pref='JU'
	*) No hay otro metodo de busqueda
	sele f13
	locate for f13->d13juri==alltrim(m.d1cla)
	if !eof('f13')
		=advsuave('Existen Anticipos y Retenciones para la Jurisdicci๓n...')
		return
	endif
endif

do clav01
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
proc clav01
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
if w_pref$'RV.RC.RP.RS'
	m.d1cla=strcero(m.d1cla)
	show get m.d1cla
endif

if w_recn<>0
	w_recn=recno(w_alias)
endif

w_orde=order(w_alias)
set orde to 'd1tip' in (w_alias)

if seek(w_pref+&w_clave,w_alias)

	if w_recn<>recno(w_alias) .or. .not. st_rlock(w_alias)
		do work01
		w_recn=recno(w_alias)
	endif
	if w_call
		unlock in (w_alias)
		clear read
	endif

else

	if w_recn<>recno(w_alias)
		w_oclav=m.d1cla
		do work01
		w_recn=recno(w_alias)
		m.d1cla=w_oclav
		show get m.d1cla
	endif

endif

set orde to (w_orde) in (w_alias)
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc work01
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
select (w_alias)
scatter memvar
m.d1tip=w_pref

if w_rdlevel<>rdlevel() and iif(version()='Visual Fox', !w_call,.t.)	&& ***vfp***
	=valid01(.t.)
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
proc when01
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
w_retu=.t.
w_vari=varread()
p_curobj=_curobj

=irclave('D1CLA','m.d1des')

RETURN

if !w_imco or !w_pref$'RC.RV.RP.RS'
	show get m.d1cimp1 disable
	show get m.d1cimp2 disable
	show get m.d1cimp3 disable
	show get m.d1cimp4 disable
	show get m.d1cimp5 disable
	show get m.d1cimp6 disable
	show get m.d1cimp7 disable
	show get m.d1cimp8 disable
endif

*if w_pref='RC'
*	show get m.d1tasa disable
*endif


return

return(w_retu)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
func valid01
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
parameters w_show

***vfp***
if version()='Visual Fox' and (w_pfile!='01' or (!wexist('IWS001') and wontop()!='IWS001' and wontop()!='IWS001B' and wontop()!='IWS001BC' and wontop()!='IWS001R' and wontop()!='IWS001RC' and wontop()!='IWS001C' and wontop()!='IWS001J')) and w_show
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

w_sosretu	= .T.		&& *===> para guardar w_retu antes de usar la FHELP xa validar

w_vari=varread()
w_vread='m.'+w_vari

IF w_pref $ 'RP.RS'
				SHOW GET m.d1alic DISABLE
				IF w_pref = 'RS'
							SHOW GET m.d1tasa DISABLE
				ENDIF
				IF w_imco
						show get m.d1cimp1 disable
						show get m.d1cimp2 disable
						show get m.d1cimp4 disable
						show get m.d1cimp5 disable
						show get m.d1cimp6 disable
						show get m.d1cimp7 disable
						show get m.d1cimp8 disable
						DO buscaret
				ENDIF
ENDIF


*===> Valida que clave no est้ en blanco o en ceros.
IF w_vari = "D1CLA"	.OR. w_show
	IF EMPTY(m.d1cla)	.OR.  STRTRAN(m.d1cla," ","0") = "000000"
		IF !w_show
			WAIT WIND "La clave no puede estar vacํa o ser nula."
		ENDIF
		w_retu = .F.
	ENDIF
ENDIF			

*===> Valida que nombre no est้ en blanco
IF w_vari = "D1DES"	.OR. w_show
	IF EMPTY(m.d1des)
		IF !w_show
			WAIT WIND "El rubro no puede estar en blanco."
		ENDIF
		IF w_retu
				_curobj	= OBJNUM(m.d1des)
				w_retu = .F.
		ENDIF
	ENDIF
ENDIF			

do case
case w_pref$'RC.RV'
	if w_vari='D1TASA' or (w_pref='RV' and w_show)
		if !m.d1tasa$'1234'
			if !w_show
				wait wind 'Opciones vแlidas: 1,2,3,4'
			endif
			IF w_retu						&& *===> Posiciona el cursor en el campo
					_curobj	= objnum(m.d1tasa)
					w_retu = .F.
			ENDIF

		endif
	endif

	if w_vari='D1CLAS' or w_show
		w_sosretu	= w_retu				&& *===> Salvo el w_retu
		do fhelp with 'D1CLAS','CL','iwp001t','f01t','clav','desc','Clase de Operaci๓n Inexistente',(p_char),'w_retu'

		IF w_sosretu	.AND. !w_retu		&& *===> Posiciona el cursor en el campo
					_curobj = OBJNUM(m.d1clas)
		ENDIF				
	endif

	if w_vari='D1ALIC'  or (w_pref$'RV.RC' and w_show)
		w_sosretu	= w_retu				&& *===> Salvo el w_retu
		do fhelp with 'D1ALIC','','iwp023','f23','d23codi','d23desc','Alํcuota Inexistente',(p_strc),'w_retu'

		IF w_sosretu	.AND. !w_retu		&& *===> Posiciona el cursor en el campo
					_curobj = OBJNUM(m.d1alic)
		ENDIF				
	endif

*	if w_imco and (w_vari='D1CIMP1' or w_show)
	if w_imco and (w_vari='D1CIMP1' or (w_show	AND 	w_pref $ 'RV.RC'))
		&& R.03b Mariano
		if empty(m.d1cimp1) and lastkey()!=p_f4	&& R.04 Mariano (agregue and lastkey()!=p_f4)
			w_imp1=''
			show get w_imp1
		else
		&& R.03e Mariano
			w_sosretu	= w_retu				&& *===> Salvo el w_retu
			do fhelp with 'D1CIMP1','','cgp001c','f01c','d1cuen','d1desc','Cuenta Inexistente',(p_char),'w_retu'
			if !f01c->d1hijo=0
				wait wind 'Cuenta no imputable...'
				w_retu=.f.
			endif

			IF w_sosretu	.AND. !w_retu	AND !w_show		&& *===> Posiciona el cursor en el campo
				_curobj = OBJNUM(m.d1cimp1)
			ENDIF				
		endif	&& R.03 Mariano
	endif

*	if w_imco and (w_vari='D1CIMP2' or w_show)
	if w_imco and (w_vari='D1CIMP2' or (w_show	AND 	w_pref $ 'RV.RC'))
		&& R.03b Mariano
		if empty(m.d1cimp2) and lastkey()!=p_f4	&& R.04 Mariano (agregue and lastkey()!=p_f4)
			w_imp2=''
			show get w_imp2
		else
		&& R.03e Mariano
			w_sosretu	= w_retu				&& *===> Salvo el w_retu
			do fhelp with 'D1CIMP2','','cgp001c','f01c','d1cuen','d1desc','Cuenta Inexistente',(p_char),'w_retu'
			if !f01c->d1hijo=0
				wait wind 'Cuenta no imputable...'
				w_retu=.f.
			endif

			IF w_sosretu	.AND. !w_retu	AND !w_show		&& *===> Posiciona el cursor en el campo
				_curobj = OBJNUM(m.d1cimp2)
			ENDIF				
		endif	&& R.03 Mariano
	endif

*	if w_imco and (w_vari='D1CIMP4' or w_show)
	if w_imco and (w_vari='D1CIMP4' or (w_show	AND 	w_pref $ 'RV.RC'))
		&& R.03b Mariano
		if empty(m.d1cimp4) and lastkey()!=p_f4	&& R.04 Mariano (agregue and lastkey()!=p_f4)
			w_imp4=''
			show get w_imp4
		else
		&& R.03e Mariano
			w_sosretu	= w_retu				&& *===> Salvo el w_retu
			do fhelp with 'D1CIMP4','','cgp001c','f01c','d1cuen','d1desc','Cuenta Inexistente',(p_char),'w_retu'
			if !f01c->d1hijo=0
				wait wind 'Cuenta no imputable...'
				w_retu=.f.
			endif

			IF w_sosretu	.AND. !w_retu	AND !w_show		&& *===> Posiciona el cursor en el campo
				_curobj = OBJNUM(m.d1cimp4)
			ENDIF				
		endif	&& R.03 Mariano
	endif

*	if w_imco and (w_vari='D1CIMP5' or w_show)
	if w_imco and (w_vari='D1CIMP5' or (w_show	AND 	w_pref $ 'RV.RC'))
		&& R.03b Mariano
		if empty(m.d1cimp5) and lastkey()!=p_f4	&& R.04 Mariano (agregue and lastkey()!=p_f4)
			w_imp5=''
			show get w_imp5
		else
		&& R.03e Mariano
			w_sosretu	= w_retu				&& *===> Salvo el w_retu
			do fhelp with 'D1CIMP5','','cgp001c','f01c','d1cuen','d1desc','Cuenta Inexistente',(p_char),'w_retu'
			if !f01c->d1hijo=0
				wait wind 'Cuenta no imputable...'
				w_retu=.f.
			endif
	
			IF w_sosretu	.AND. !w_retu	AND !w_show		&& *===> Posiciona el cursor en el campo
				_curobj = OBJNUM(m.d1cimp5)
			ENDIF				
		endif	&& R.03 Mariano
	endif
endcase

DO CASE
case w_pref$'RC.RV.RP.RS'
	if w_imco and (w_vari='D1CIMP3' or w_show)
		&& R.03b Mariano
		if empty(m.d1cimp3) and lastkey()!=p_f4	&& R.04 Mariano (agregue and lastkey()!=p_f4)
			w_imp3=''
			show get w_imp3
		else
		&& R.03e Mariano
			w_sosretu	= w_retu				&& *===> Salvo el w_retu
			do fhelp with 'D1CIMP3','','cgp001c','f01c','d1cuen','d1desc','Cuenta Inexistente',(p_char),'w_retu'
			if !f01c->d1hijo=0
				wait wind 'Cuenta no imputable...'
				w_retu=.f.
			endif

			IF w_sosretu	.AND. !w_retu	AND !w_show		&& *===> Posiciona el cursor en el campo
				_curobj = OBJNUM(m.d1cimp3)
			ENDIF				
		endif	&& R.03 Mariano
	endif

*	if w_imco and (w_vari='D1CIMP6' or w_show)
	if w_imco AND	(w_vari='D1CIMP6' OR												;
					(w_show	AND 	w_pref $ 'RV.RC')	OR								;
					(w_show	AND 'M.D1CIMP6' $ w_pcimp)	 									)

		&& R.03b Mariano
		if empty(m.d1cimp6) and lastkey()!=p_f4	&& R.04 Mariano (agregue and lastkey()!=p_f4)
			w_imp6=''
			show get w_imp6
		else
		&& R.03e Mariano
			w_sosretu	= w_retu				&& *===> Salvo el w_retu
			do fhelp with 'D1CIMP6','','cgp001c','f01c','d1cuen','d1desc','Cuenta Inexistente',(p_char),'w_retu'
			if !f01c->d1hijo=0
				wait wind 'Cuenta no imputable...'
				w_retu=.f.
			endif
	
			IF w_sosretu	.AND. !w_retu	AND !w_show		&& *===> Posiciona el cursor en el campo
				_curobj = OBJNUM(m.d1cimp6)
			ENDIF				
		endif	&& R.03 Mariano
	endif

*	if w_imco and (w_vari='D1CIMP7' or w_show)
	if w_imco AND	(w_vari='D1CIMP7' OR												;
					(w_show	AND 	w_pref $ 'RV.RC')	OR								;
					(w_show	AND 'M.D1CIMP7' $ w_pcimp)										)

		&& R.03b Mariano
		if empty(m.d1cimp7) and lastkey()!=p_f4	&& R.04 Mariano (agregue and lastkey()!=p_f4)
			w_imp7=''
			show get w_imp7
		else
		&& R.03e Mariano
			w_sosretu	= w_retu				&& *===> Salvo el w_retu
			do fhelp with 'D1CIMP7','','cgp001c','f01c','d1cuen','d1desc','Cuenta Inexistente',(p_char),'w_retu'
			if !f01c->d1hijo=0
				wait wind 'Cuenta no imputable...'
				w_retu=.f.
			endif
	
			IF w_sosretu	.AND. !w_retu	AND !w_show		&& *===> Posiciona el cursor en el campo
				_curobj = OBJNUM(m.d1cimp7)
			ENDIF				
		endif	&& R.03 Mariano
	endif

*	if w_imco and (w_vari='D1CIMP8' or w_show)
	if w_imco AND	(w_vari='D1CIMP8' OR												;
					(w_show	AND 	w_pref $ 'RV.RC')	OR								;
					(w_show	AND 'M.D1CIMP8' $ w_pcimp)										)

		&& R.03b Mariano
		if empty(m.d1cimp8) and lastkey()!=p_f4	&& R.04 Mariano (agregue and lastkey()!=p_f4)
			w_imp8=''
			show get w_imp8
		else
		&& R.03e Mariano
			w_sosretu	= w_retu				&& *===> Salvo el w_retu
			do fhelp with 'D1CIMP8','','cgp001c','f01c','d1cuen','d1desc','Cuenta Inexistente',(p_char),'w_retu'
			if !f01c->d1hijo=0
				wait wind 'Cuenta no imputable...'
				w_retu=.f.
			endif

			IF w_sosretu	.AND. !w_retu	AND !w_show		&& *===> Posiciona el cursor en el campo
				_curobj = OBJNUM(m.d1cimp8)
			ENDIF				
		endif	&& R.03 Mariano
	endif

otherwise &&(es el screen de jurisdicciones)


	if w_vari='D1TDGI' .or. w_show
		if !empty(m.d1tdgi) or lastkey()=p_f4
			do fhelp with 'D1TDGI','JD','iwp001t','f01t','clav','desc','Jurisdicci๓n DGI Inexistente',(p_strc),'w_retu'
			m.d1tdgi=strcero(m.d1tdgi)
			show get m.d1tdgi
		else
*			m.d1tdgi=strcero(m.d1tdgi)
			w_tdgi=''
			show get m.d1tdgi
			show get w_tdgi
WAIT WIND "DEBE INGRESAR JURISDICCION AFIP"
		endif
	endif


endcase


=ultdepri('D1CLA','m.'+x_ultget)	&& R.01 Mariano (w_ultget por x_ultget)
=prideult(x_ultget,'m.'+x_ultget)	&& R.01 Mariano (w_ultget por x_ultget)

=modicampo('D1CLA')


*===> Puede ser que la ๚ltima validaci๓n est้ OK y hubo alguna inconsistencia antes
IF !w_sosretu	
		w_retu	= .F.
ENDIF
*===>		

return(w_retu)

***vfp***
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc actu01
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	on key label f9 do actu01x
	keyboard '{f9}'
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc actu01x
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

if !valid01(.t.)
	wait 'Debe completar datos...' wind
	return
endif

do clav01
if .not. eof(w_alias) .and. .not. st_rlock(w_alias)
	return
endif

=db_capt(w_alias)
w_orde=order(w_alias)
set orde to 'd1tip' in (w_alias)

if .not. seek(w_pref+&w_clave,w_alias)

	if w_agre
		wait 'Agregando registro...' wind time .4
		if begintran()
			=net_appe(w_alias)
			sele (w_alias)
			gather memvar
			
			if w_pref='JU'	&& estoy en jurisdicciones
				=db_rlock('f00')
				if !empty(m.d1coef) and !empty(f00.p0poce)	&& tengo coeficientes y descuento no puede ser
					=set_alia('f00')
					replace f00.p0poce with 0	&& borro el porcentaje de descuento si tengo coeficientes
					=rest_alia()
				endif
				unlock in f00
			endif
			
			if w_pref='RV.RC'	&& estoy en rubros
				if seek(m.d1alic,'f23')
					=db_rlock('f23')
					=set_alia('f23')
					replace f23.d23fusa with .t.
					=rest_alia()
				endif
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
				sele (w_alias)
				gather memvar
			
				if w_pref='JU'	&& estoy en jurisdicciones
					=db_rlock('f00')
					if !empty(m.d1coef) and !empty(f00.p0poce)	&& tengo coeficientes y descuento no puede ser
						=set_alia('f00')
						replace f00.p0poce with 0	&& borro el porcentaje de descuento si tengo coeficientes
						=rest_alia()
					endif
					unlock in f00
				endif
			
				if w_pref='RV.RC.RP.RS'	&& estoy en rubros
					if seek(m.d1alic,'f23')
						=db_rlock('f23')
						=set_alia('f23')
						replace f23.d23fusa with .t.
						=rest_alia()
					endif
				endif
		
				=endtran()
			endif
			w_modicampo=.f.
		endif
	else
		do work01
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
proc brow01
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
push menu _msysmenu
=skipbar('Registro','Insertar' ,!w_call)
=skipbar('Registro','Agregar'	 ,.t.)
=skipbar('Registro','Eliminar' ,.t.)
=skipbar('Registro','Modificar',.t.)
=skipbar('Registro','Consultar',.t.)
=skipbar('Registro','Elegir'	 ,.t.)

w_orde=order(w_alias)
set orde to 'd1des' in (w_alias)
if eof(w_alias)
	go top in (w_alias)
endif

sele (w_alias)
if w_call
	set filter to d1tip$w_pref
endif

do case
	case wexist('iws005i') and w_pref='JU'
		browse fields;
			d1cla :h='C๓digo'		 :p='@K':v=fenter():f,;
			d1des :h='Descripci๓n':p='@K' :r,;
			d1coef :h='Coeficiente' :p='999.999' :r ;
			nome noap noed node norm in screen title (w_title) font 'arial',10;
			valid :f iif(w_call,.t.,salebrow(lastkey()))
	otherwise
		browse fields;
			d1cla :h='C๓digo'		 :p='@K':v=fenter():f,;
			d1des :h='Descripci๓n':p='@K';
			nome noap noed node norm in screen title (w_title) font 'arial',10;
			valid :f iif(w_call,.t.,salebrow(lastkey()))
endcase

if !w_call
	=work01()
else
	set filter to
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
proc busc01
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
w_orde=order(w_alias)
do busca
if .not. eof(w_alias)
	do work01
endif
set orde to (w_orde) in (w_alias)
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc orde01
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
do case
case prompt()='Ordenado por C๓digo'
	set orde to 'd1tip' in (w_alias)
case prompt()='Ordenado por Descripci๓n'
	set orde to 'd1des' in (w_alias)
endcase
if wexist(w_title)
	show wind (w_title) refresh
endif
return

*======================================================================================
PROCEDURE buscaret

pc_pr	= ""							&&		f00.pr {c/v} {1/2/3}
w_pcimp1	= " "
w_pcimp2	= " "
w_pcimp3	= " "

pn_n	= 4								&&		= 4/6/7/8		4 = SIN ASIGNAR
pn_m	= 4
pn_l	= 4

*f00.p0pr{c/v}[n]							n = 1, 2, 3
*					= 1 va en {c/v}im6
*					= 2 va en {c/v}im7
*					= 3 va en {c/v}im8					

IF w_pref $ 'RV.RS'
			pc_pr	= "f00.p0prv"
ELSE
	pc_pr	= "f00.p0prc"
ENDIF

&& Ingresos Brutos
						FOR pn_n = 1 TO 3
								pc_pr	= SUBSTR(pc_pr,1,9) + STR(pn_n,1)
								IF &pc_pr = 1
										pn_n = pn_n + 4
								ENDIF
						ENDFOR
IF pn_n <> 4
		w_pcimp1	= 'M.D1CIMP' + STR(pn_n,1)
		SHOW GET &w_pcimp1 ENABLE
ENDIF


&& Ganancias
						FOR pn_m = 1 TO 3
								pc_pr	= SUBSTR(pc_pr,1,9) + STR(pn_m,1)
								IF &pc_pr = 2
										pn_m = pn_m + 4
								ENDIF
						ENDFOR

IF pn_m <> 4
		w_pcimp2	= 'M.D1CIMP' + STR(pn_m,1)
		SHOW GET &w_pcimp2 ENABLE
ENDIF

&& Otros
						FOR pn_l = 1 TO 3
								pc_pr	= SUBSTR(pc_pr,1,9) + STR(pn_l,1)
								IF &pc_pr = 3
										pn_l = pn_l + 4
								ENDIF
						ENDFOR

IF pn_l <> 4
		w_pcimp3	= 'M.D1CIMP' + STR(pn_l,1)
		SHOW GET &w_pcimp3 ENABLE
ENDIF

w_pcimp	= w_pcimp1 + '.' + w_pcimp2 + '.' + w_pcimp3 

w_a	= MAX(pn_n, pn_m, pn_l)

x_ultget	= 'D1CIMP' +	STR(w_a,1)	&& R.01 Mariano (w_ultget por x_ultget)

RETURN





