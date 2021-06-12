import React from 'react';
import SourceCode from './SourceCode';

function Editor(props) {

  const fixupLang = (lang) => {
    if (lang == "Haskell") return "haskell";
    if (lang == "X86") return "x86asm";
    if (lang == "C") return "c'";
    if (lang == "LLVM") return "llvm";
  }

  return (
    <div>
      <div id='editor-area'>
        <SourceCode
          source={props.source}
          id={'source-in'}
          language={fixupLang(props.source_in)}
        />

        <div id='middle-panel'>
        </div>

        <SourceCode
          source={props.target}
          id={'source-out'}
          language={fixupLang(props.source_out)}
        />
      </div>
    </div>
  );
}

export default Editor;
