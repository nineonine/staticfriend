import React from 'react';

function InsightPanel(props) {
    return (
        <div id={'insight-panel'}>
            {props.children}
        </div>
    );
}

export default InsightPanel;
