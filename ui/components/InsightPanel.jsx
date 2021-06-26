import React from 'react';

function InsightPanel(props) {
    const toggleHeight = (fix) => fix ? '300px' : '30px';
    return (
        <div id={'insight-panel'} style={{'height': toggleHeight(props.fixed_insight_panel)}}>
            {props.children}
        </div>
    );
}

export default InsightPanel;
