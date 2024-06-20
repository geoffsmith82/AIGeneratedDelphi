unit uTetrisShapes;

interface

type
  TShape = array[0..3, 0..3] of Boolean;

const
  Shapes: array[0..6] of TShape = (
    // I shape
    ((False, False, False, False),
     (True,  True,  True,  True),
     (False, False, False, False),
     (False, False, False, False)),
    // J shape
    ((True,  False, False, False),
     (True,  True,  True,  False),
     (False, False, False, False),
     (False, False, False, False)),
    // L shape
    ((False, False, True,  False),
     (True,  True,  True,  False),
     (False, False, False, False),
     (False, False, False, False)),
    // O shape
    ((True,  True,  False, False),
     (True,  True,  False, False),
     (False, False, False, False),
     (False, False, False, False)),
    // S shape
    ((False, True,  True,  False),
     (True,  True,  False, False),
     (False, False, False, False),
     (False, False, False, False)),
    // T shape
    ((False, True,  False, False),
     (True,  True,  True,  False),
     (False, False, False, False),
     (False, False, False, False)),
    // Z shape
    ((True,  True,  False, False),
     (False, True,  True,  False),
     (False, False, False, False),
     (False, False, False, False))
  );

implementation

end.
