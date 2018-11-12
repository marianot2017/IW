*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP964.PRG -> Exportar Percep./Retenc. AFIP Ing. Brutos
*										VENTAS
*)-H
*)-H Fecha de inicio :
*)-H Fecha de fin    :
*)-H
*)-H Updates
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
set skip of menu _msysmenu .t.

*) Variables internas----------------------------------------------------------

w_pfile='964'
w_prog =upper(prompt())
w_title=upper('Exportar Per/Ret Ventas I.B. Capital')
w_alta =.f.
w_alias='f05'

*) Apertura de archivos--------------------------------------------------------

fieldvec[1]='F00'
fieldvec[2]='F03'
fieldvec[3]='F04'
fieldvec[4]='F05'
*fieldvec[5]='F06'
*fieldvec[6]='F24'

use (d0f00) in 0 alias 'f00'								&&(Parametros)
use (d0f02) in 0 alias 'f02' order 'd2clie'  		&&(Clientes)
use (d0f03) in 0 alias 'f03' order 'd3prov'  		&&(Proveedores)
use (d0f04) in 0 alias 'f04' order 'd4peri'			&&(Periodos)
use (d0f05) in 0 alias 'f05' order 'd5peri'			&&(Comprobantes)
*use (d0f06) in 0 alias 'f06' order 'd6inte'			&&(Clientes Eventuales)
*use (d0f24) in 0 alias 'f24' order 'd24codi' 		&&(Percep./Retenc./Otros)

if neterr()
	pop key
	pop menu _msysmenu
	fieldvec=''
	=closall()
	return
endif

fieldvec=''

sele (w_alias)

*) Variables-------------------------------------------------------------------

go bottom in f04
wdperi=f04.d4peri


*===> Es solo VENTAS

w_cpra	= .F.
w_pecp	= .F.
w_recp	= .F.

w_vta	= .T.
w_pevt	= .T.
w_revt	= .T.

w_dire	= " "

DO dirs

*)-----------------------------------------------------------------------------

do ssm998.mpr

do iws964.spr


pop key
pop menu _msysmenu
=closall()

dele file &d0f99.b.dbf
dele file &d0f99.c.dbf
DELE FILE ivf099b.idx

return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC Cons964
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
if .not. valid964(.t.)
	wait wind 'Verifique los datos ingresados...'
	return
endif

if !w_pecp and !w_recp and !w_pevt and !w_revt
	wait window 'Debe elegir Percepciones y/o Retenciones'
	IF !w_pecp 
			_curobj = objnum(w_pecp)
	ELSE
		_curobj = objnum(w_recp)
	ENDIF
	w_retu = .F.		
	SHOW GETS
	return
endif

if w_pecp or w_recp or w_pevt or w_revt
	=gentmp()

	if used('f99b')
		sele f99b
		use
		dele file &d0f99.b.dbf
	endif

	sele 0
	
*	if w_pecp or w_recp
*		w_cpra=.t.
*	endif
*	if w_pevt or w_revt
*		w_vta=.t.
*	endif

	select *;		
		from f05;
		where d5peri = wdperi ;
		and d5tipo = 'V'		;
		and !empty(d5cpib)  ;
		into dbf &d0f99.b

	use &d0f99.b alias f99b exclusive

	set relation to d5clie into f03
	set relation to d5clie into f02 additive	
*	set relation to d5inte into f06 additive
*	set relation to d5pere into f24 additive
 	go top
	if eof()
		wait window 'Sin movimientos para informar' time 1
		return
	endif
endif

if w_pevt or w_revt	
	if used('f99c')
		sele f99c
		use
		dele file &d0f99.c.dbf
	endif

	sele 0

	create dbf &d0f99.c (signo C(1), agente c(7), a๑o c(2), mes c(2), dia c(2), ;
		regimen c(1), codigo c(3), isib c(12), cuit c(13), importe n(15,2) )
	use &d0f99.c alias f99c
	go top in f99b
	do while !eof('f99b')
*		if f99b.d5tipo='V' and (iif(w_pevt,f99b.d5regi='P',.f.) or 			;
*			iif(w_revt,f99b.d5regi='R',.f.) or f99b.d5tico='R')
		if f99b.d5tipo='V' and (iif(w_pevt,f99b.d5regi='P',.f.) or 			;
			iif(w_revt,f99b.d5regi='R',.f.) )
			
			m.agente=f00.p0nagp+'-'+f00.p0dagp
			m.a๑o=right(dtoc(f99b.d5fech),2)
			m.mes=substr(dtoc(f99b.d5fech),4,2)
			m.dia=left(dtoc(f99b.d5fech),2)
			m.regimen=iif(f99b.d5tico='R','R',f99b.d5regi)
			m.codigo=str(val(f99b.d5cpib),3)
			m.isib=f02.d2nuib
			m.cuit=f02.d2cuit
			w_campo=''
			for i=1 to 3
				w_cprv='f00.p0prv'+(str(i,1))
				if &w_cprv=1   && IB
					w_campo=w_cprv
					exit
				endif						
			endfor					
			w_campo=iif(!empty(w_campo),str(val(right(w_campo,1))+5,1),'0')
			w_campo=iif(w_campo>'0','f99b.d5imp'+w_campo,'0')
*===>
*			m.importe=iif(f99b.d5tico='R',abs(f99b.d5tota),abs(eval(w_campo)))
			m.importe=iif(f99b.d5tico='R',f99b.d5tota,eval(w_campo))

			IF m.importe >= 0
						m.signo = "+"
			ELSE
				m.signo = "-"
			ENDIF										
			
		
			append blank
			gather memvar		
		endif			
		skip in f99b
	enddo
	do agrupavta
	go top

	if eof('f99c')
		wait window 'Sin movimientos para informar' time 1
	else
		w_retu=creadir(curdir()+'AFIP')
		if w_retu=0 or w_retu=6

			COPY TO &w_dire FIELDS EXCEPT signo TYPE SDF
			
			do repdest &&(Setea variables internas del reporte)

			go top
			if !empty(p_repclaus) &&(debe ser preview o to printer)
				p_report='iwr964'
				=rep_form()
				p_report=''
			endif

			do represet &&(Resetea estado)
		endif
	endif
endif


return


*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC when964
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
w_retu=.t.
w_vari=varread()
p_curobj=_curobj

return(w_retu)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC valid964
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_show

if empty(parameters())
	w_show=.f.
endif

w_retu=.t.
w_vari=varread()
w_vread='m.'+w_vari

if w_vari='WDPERI' or w_show
	if lastkey()=p_f4
		do fhelp with 'WDPERI','','iwp004','f04','d4peri','d4peri','',(p_char),'w_retu'
	endif

*===>
	w_fech	= CTOD('01/' + wdperi)
	IF trdate('w_fech')
			wdperi	= RIGHT(DTOC(w_fech),7)
			SHOW GET wdperi
	ELSE
		IF !w_show
				WAIT WIND "El perํodo no es correcto"
		ENDIF
		w_retu	= .F.
		_curobj	= objnum(wdperi)
	ENDIF
*===>
	IF w_retu
			wkperi = PtoI(wdperi)

			if ! seek(wkperi,'f04')
					if !w_show
							wait window 'Perํodo Inexistente'
					endif
					w_retu=.f.
					_curobj	= objnum(wdperi)
			endif
	ENDIF
	DO dirs
endif

if w_vari='W_PEVT'
	=mchkbox(@w_pevt)
endif

if w_vari='W_PEVT' or w_show
			DO dirs
endif
		
if w_vari='W_REVT'
	=mchkbox(@w_revt)
endif

if w_vari='W_REVT' or w_show
			DO dirs
			if (!w_pevt and !w_revt)
						wait window 'Debe elegir Percepciones y/o Retenciones de IB'
						IF !w_pevt 
								_curobj = objnum(w_pevt)
						ELSE
							_curobj = objnum(w_revt)
						ENDIF
						w_retu = .F.		
						SHOW GETS
			endif
endif

return (w_retu)

************************************************************
proc agrupavta	 && Agrupa comprobantes de ventas iguales
************************************************************

private w_cant, w_recno

index on a๑o+mes+dia+regimen+codigo+isib+cuit tag agrupa
go top
do while !eof('f99c') 
	scatter memvar
	w_recno=recno('f99c')

	** acumulo el comprobante

	sum importe to m.importe ;
		while m.a๑o+m.mes+m.dia+m.regimen+m.codigo+m.isib+m.cuit=a๑o+mes+dia+regimen+codigo+isib+cuit
			
	go w_recno in f99c
	gather memvar
	
	skip in f99c

	do 	while m.a๑o+m.mes+m.dia+m.regimen+m.codigo+m.isib+m.cuit=a๑o+mes+dia+regimen+codigo+isib+cuit and !eof('f99c')
		delete		
		skip in f99c
	enddo

enddo

return

************************************************************
function tipcomprob
************************************************************
parameter w_ttab
private w_tafi
do case
	case w_ttab$'01-05-06-10-11-14-16-19-22-30-34-35-36-39-40-41-51'+ ;
			'60-61-62-63-64-65-80-81-82-83-84-87'
		w_tafi='F'
	case w_ttab$'04-09-15-70'
		w_tafi='R'
	case w_ttab$'02-07-12-20-37-86-94'
		w_tafi='D'
	case w_ttab$'03-08-13-21-38-85-95'
		w_tafi='C'
	case w_ttab$'92-93-50-51-52-53-54-55-56-57-58-59'
		w_tafi=' '
	otherwise
		w_tafi='F'						
endcase
return w_tafi

************************************************************
function letcomprob
************************************************************
parameter w_ltab
private w_lafi
do case
	case w_ltab$'A'
		w_lafi='A'
	case w_ltab$'B'
		w_lafi='B'
	case w_ltab$'C'
		w_lafi='C'
	otherwise
		w_lafi=' '						
endcase
return w_lafi

*===============================================================================
PROCEDURE dirs

IF w_pevt OR w_revt
		w_dire=curdir()+'AFIP\INGBRU\CAPITAL\PR'+ f77.d77empr + right(ptoi(wdperi),4)+'.TXT'
ELSE
	w_dire=''
ENDIF

SHOW GET w_dire

RETURN

*================================================================================
***puesto adrede para que incluya el report al proyecto**
report form iwr964
