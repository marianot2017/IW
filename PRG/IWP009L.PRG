*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP009L.PRG -> Listador de facturas tipo x cliente
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

*) Variables del report--------------------------------------------------------

w_pfile='09l'
w_alias='f09'
w_title=db_name(w_alias)

w_clde=space(5)
w_clha='99999'

w_drep=''
w_hrep=''

*) Apertura de archivos--------------------------------------------------------

fieldvec[1]='F00'
fieldvec[2]='F01'
fieldvec[3]='F02'
fieldvec[4]='F04'
fieldvec[5]='F08'
fieldvec[6]='F09'
fieldvec[7]='F20'

use (d0f00) in 0 alias 'f00'						&&(Parametros)
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

fieldvec=''

*)-----------------------------------------------------------------------------

do ssm998.mpr
do iws009l.spr

pop key
pop menu _msysmenu
=closall()

dele file &d0f99.c.dbf

return

***vfp***
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons09l
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	on key label f9 do cons09l0
	keyboard '{f9}'
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons09l0
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
on key label f9
***vfp***

if .not. valid09l(.t.)
	wait wind 'Verifique los datos ingresados...'
	return
endif

=gentmp()

w_drep=w_clde
w_hrep=w_clha

if used('f99c')
	sele f99c
	use
	dele file &d0f99.c.dbf
endif

sele 0
select *;
	from f09;
	where between(d9clie,w_clde,w_clha);
	into dbf &d0f99.c order by d9clie
use &d0f99.c alias f99c exclusive
	
if !p_cancela
	return
endif

do repdest &&(Setea variables internas del reporte)

sele f99c
set rela to d9clie into f02
set rela to d9codi into f08 additive
set rela to 'RV'+d9rubr into f01 additive
go top
if !empty(p_repclaus) &&(debe ser preview o to printer)
	p_report='iwr009l'
	=rep_form()
	p_report=''
endif

do represet &&(Resetea estado)

sele f99c
set relation to
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC valid09l
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_show

if empty(parameters())
	w_show=.f.
endif

PRIVATE w_retu,w_vari
w_retu=.t.
w_vari=varread()

if w_vari='W_CLDE' or w_show
	w_clde=strcero(w_clde)
	if lastk()=p_f4
		do fhelp with 'W_CLDE','','iwp002','f02','d2clie','d2clie','',(p_strc),'w_retu'
	endif
endif

if w_vari='W_CLHA' or w_show
	w_clha=strcero(w_clha)
	if lastk()=p_f4
		do fhelp with 'W_CLHA','','iwp002','f02','d2clie','d2clie','',(p_strc),'w_retu'
	endif
	if w_clde>w_clha
		if !w_show
			wait wind 'Hasta debe ser mayor o igual que desde...'
		endif
		w_retu=.f.
	endif
endif
return(w_retu)

***puesto adrede para que incluya el report al proyecto**
report form iwr009l
