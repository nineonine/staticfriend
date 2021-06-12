import React from 'react';
import SourceCode from './SourceCode';

function Editor(props) {

  // https://github.com/react-syntax-highlighter/react-syntax-highlighter/blob/master/AVAILABLE_LANGUAGES_HLJS.MD
  const fixupLang = (lang) => {
    if (lang == "Haskell") return "haskell";
    if (lang == "X86") return "x86asm";
    if (lang == "C") return "c";
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
          language={fixupLang(props.target_out)}
          update_target_loc={props.update_target_loc}
        />
      </div>
    </div>
  );
}

export default Editor;
