*-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*-H Programa        : IWP094.PRG -> Funciones
*-H Release			: 02
*-H Fecha de inicio :
*-H Fecha de fin    :
*-H
*-H Updates
*-H
*-H  Fecha       Programmer      Comentarios
*-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_modo
PRIVATE all like w_*

** Salvado de estados anteriores

push key clear      && Salva estados de teclas
push menu _msysmenu && Salva estado de menu

set skip of menu _msysmenu .t.

** Apertura de archivos **

fieldvec[1]='F94 - Documentaci๓n'
use (d0f94) in 0 alias f94 order tag d94desc && funciones

if neterr()
	pop key
	pop menu _msysmenu
	fieldvec=''
	=closall()
	return
endif

fieldvec=''

w_pfile='94'

w_prog=upper(prompt())

w_alias='f94'

w_title=db_name(w_alias)

sele (w_alias)

w_alta=.f.

do ssm998.mpr

=skipbar('Registro','Confirmar', .t.)
=skipbar('Registro','Elegir', .t.)


w_orde=1
w_tema=d94desc+'     '+d94docu

do iws094.spr

pop key
pop menu _msysmenu

=closall()

read when .f.

return

*-P อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
Procedure Valid94
*-P อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
w_retu=.t.
w_vari=varread()

if w_vari='W_TEMA'
	w_rec=left(w_tema,25)
	do cons94
endif

return w_retu

*-P อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
Procedure Cons94
*-P อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

sele (w_alias)
=seek(w_rec,'f94')
if found()
*	=winword(alltrim(d94docu))	&& R.01 Mariano
	=winpdf(alltrim(d94docu))	&& R.01 Mariano
	_curobj=objnum(w_tema)	&& R.02 Mariano
endif
go top in &w_alias

return

*-P อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
Procedure xxCons94
*-P อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

sele (w_alias)
=seek(w_rec,'f94')
if found()

	keyboard '{enter}'
	keyboard '{enter}'

	modify general d94text

endif
go top in &w_alias

return
