(function(global){
  if(!global.Chessboard2) return;
  var orig = global.Chessboard2;
  global.Chessboard2 = function(element, config){
    if(config && config.pieceTheme){
      if(typeof config.pieceTheme === 'function'){
        global.pieceTheme = config.pieceTheme;
      } else {
        var theme = config.pieceTheme;
        global.pieceTheme = function(piece){
          return theme.replace('{piece}', piece);
        };
      }
    }
    return orig(element, config);
  };
})(window);
