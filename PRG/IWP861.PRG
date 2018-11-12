*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP861.PRG -> Determinacion Agente Informacion
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

w_prog=upper(prompt())
w_alta=.f.
w_pfile='861'
w_title='ANALISIS AGENTE DE INFORMACION'

*) Apertura de archivos--------------------------------------------------------

fieldvec[1]='F01'
fieldvec[2]='F02'
fieldvec[3]='F03'
fieldvec[4]='F05'
fieldvec[5]='F06'
fieldvec[6]='F12'

use (d0f01) in 0 alias 'f01' order 'd1tip'  &&(Tablas)
use (d0f02) in 0 alias 'f02' order 'd2clie' &&(Clientes)
use (d0f03) in 0 alias 'f03' order 'd3prov' &&(Proveedores)
use (d0f05) in 0 alias 'f05' order 'd5pefe' &&(Comprobantes)
use (d0f06) in 0 alias 'f06' order 'd6inte' &&(Arch.Alternativo)
use (d0f12) in 0 alias 'f12' 					  &&(parametros dgi citi)

if neterr()
	pop key
	pop menu _msysmenu
	fieldvec=''
	=closall()
	return
endif

fieldvec=''

*)-----------------------------------------------------------------------------

=SetComp() &&(Setea filtros de comprobante DB CR y su signo)
w_ano=space(4)

do ssm998.mpr
do iws861.spr

pop key
pop menu _msysmenu
=closall()

dele file &d0f99.b.dbf

return

***vfp***
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons861
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	on key label f9 do cons861x
	keyboard '{f9}'
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons861x
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
on key label f9
***vfp***

if .not. valid861(.t.)
	wait wind 'Verifique los datos ingresados...'
	return
endif

t_leye='Correspondiente al a๑o '+w_ano+', Tope Bienes: '+allt(str(f12->p0ivbi))+', Tope Locaciones: '+allt(str(f12->p0ilos))

if CreaMov('01/'+w_ano,'12/'+w_ano,'VC','1',.t.)
	if eof('f9c')
		sele f9c
		use
		dele file &d0f99.c.dbf
		dele file &d0f99.c.cdx
		=advsuave('No hay Movimientos...','Informaci๓n')
		return
	endif
endif

do list861

sele f99b
go top
do repdest &&(Setea variables internas del reporte)
if !empty(p_repclaus) &&(debe ser preview o to printer)
	p_report='iwr861'
	=rep_form()
	p_report=''
endif
do represet &&(Resetea estado)

sele f9c
use
dele file &d0f99.c.dbf
dele file &d0f99.c.cdx

return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC list861
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

if used('f99b')
	sele f99b
	zap
else
	sele 0
	create dbf &d0f99.b (d99text c(150),d99tobi n(19,2),d99tolo n(19,2),d99tota n(19,2),d99leye c(20),d99valo l, d99vato l,d99vali l)
	use &d0f99.b alias f99b exclusive
endif

stor 0 to acum1,acum2,acum3
sele f9c
go top

wflag=.f.
do while !eof()
	stor 0 to wAcumC,wAcum1,wAcum2,wAcum3

	w5nocl=d5nocl
	w5tido=d5tido

	w5tipo=d5tipo
	w5cuit=d5cuit
	do while w5tipo+w5cuit=d5tipo+d5cuit
		* Para determinar Condicion de Agente de Informacion

		* Excluye DB y CR si es necesario
		w5tota=d5tota * iif(d5coco$wComDB,wSignDB,iif(d5coco$wComCR,wSignCR,1))

		if d5tipo='V' &&(Solo ventas)
			do case
			case d5clas$'1'               &&  Bienes
				acum1 = acum1 + w5tota
			case d5clas$'2'               &&  Servicios Generales
				acum2 = acum2 + w5tota
			case d5clas$'3'               &&  Servicios Anexo1
				acum3 = acum3 + w5tota
			endcase
		endif

		* Por Cliente o Proveedor
		do case
		case d5clas$'1'               &&  Bienes
			wAcum1 = wAcum1 + w5tota
		case d5clas$'2'               &&  Servicios Generales
			wAcum2 = wAcum2 + w5tota
		case d5clas$'3'               &&  Servicios Anexo1
			wAcum3 = wAcum3 + w5tota
		endcase

		wAcumC = wAcumC + w5tota

		skip in f9c
	enddo
	
	insert into f99b;
		values (iif(w5tipo='V','Clie: ','Prov: ')+w5nocl+' '+w5cuit+' '+w5tido,wAcum1,wAcum2+wAcum3,wAcumC,'',.t.,.t.,.f.)
		
enddo

insert into f99b;
	values ('',0,0,0,'',.f.,.f.,.t.)

insert into f99b;
	values ('Acumulado por Venta de Bienes........................: ',acum1,0,0,iif(acum1>=f12->p0ivbi,'SUPERA EL TOPE','NO SUPERA EL TOPE'),.f.,.f.,.f.)

insert into f99b;
	values ('Acumulado por Locaciones de Obras y Servicios General: ',acum2+acum3,0,0,iif(acum2+acum3>=f12->p0ilos,'SUPERA EL TOPE','NO SUPERA EL TOPE'),.f.,.f.,.f.)
	
insert into f99b;
	values ('',0,0,0,'',.f.,.f.,.t.)
	
if !empty(acum1) .and. !empty(acum2+acum3)
	if acum1+acum2+acum3>=f12->p0ilos
		insert into f99b;
			values ('SUPERA EL TOPE','Existen Operaciones de Ambos Tipos (Art 2. 2do Parr.): '+ALLT(STR(acum1+acum2+acum3)),0,0,0,'',.f.,.f.,.f.)
	endif
endif
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC valid861
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_show

if empty(parameters())
	w_show=.f.
endif

PRIVATE w_retu,w_vari
w_retu=.t.
w_vari=varread()

if w_vari='W_ANO' or w_show
	if w_ano<'1990'
		if !w_show
			wait wind 'A๑o no permitido.'
		endif
		w_retu=.f.
	endif
endif
return(w_retu)

***puesto adrede para que incluya el report al proyecto**
report form iwr861
