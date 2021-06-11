import React from 'react';
import SourceCode from './SourceCode';

function Editor(props) {
  return (
    <div>
      <div id='editor-area'>
        <SourceCode
          source={props.source}
          id={'source-in'}
          language={'haskell'}
        />

        <div id='middle-panel'>
        </div>

        <SourceCode
          source={props.target}
          id={'source-out'}
          language={'x86asm'}
        />
      </div>
    </div>
  );
}

export default Editor;
