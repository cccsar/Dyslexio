# Gramática de LIPS-USB

## Gramática para la Documentación

### Entrada

    <entrada> -> <acción> | <expresion>

### Acciones

    <acción> -> <definición> | <asignación>
    <definicion> -> lazy <D>| <D>;
    <D> -> int <asignacion>; | bool <asignacion>;
    <asignación> -> <id> := <expresion>;

### Expresiones

    <expresion> -> <expresion> || <expresion> | <F>
    <F> -> <F> && <F> | <G>
    <G> -> <G> = <G> | <G> <> <G> | <H>
    <H> -> <H> < <H> | <H> <= <H> | <H> > <H> | <H> >= <H> | <I>
    <I> -> <I> + <I> | <I> - <I> | <J>
    <J> -> <J> * <J> | <J> % <J> | <K>
    <K> -> <K> ^ <K> ! <L>
    <L> -> !<expresion> | -<expresion> | +<expresion>
    <L> -> (<expresion>)
    <L> -> '<expresion>'
    <L> -> <id>
    <L> -> <constante>
    <L> -> <id>(<M>)
    <M> -> <id>, <M> | <id>

## Gramática para la Implementación

    (gramática transformada para la implementación del reconocedor)
