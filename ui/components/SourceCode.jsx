import React from 'react';
import SyntaxHighlighter from 'react-syntax-highlighter';
import { a11yDark } from 'react-syntax-highlighter/dist/esm/styles/hljs';

function SourceCode(props) {

  return (
    <div className={'source-container'} id={props.id}>
      <p className={'source-header'}>{props.language}</p>
      <div className={'syntax-highlight-container'}>
	<SyntaxHighlighter
	  language={props.language}
	  style={a11yDark}
	  showLineNumbers={true}
	  customStyle={{'height': 'inherit', 'margin':0}}
	  wrapLines={true}
	  wrapLongLines={true}
	  lineProps={(lineNumber) => ({
			style: { display: "block", cursor: "pointer" },
			onClick: () => {
				props.update_target_loc?.(lineNumber)}}
		)}
	>
	  {props.source}
	</SyntaxHighlighter>
      </div>
    </div>
  );
}

export default SourceCode;
