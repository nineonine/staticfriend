import React from 'react';

const phantomStyle = {
    display: "block",
    padding: "20px",
    height: "60px",
    width: "100%"
};

function InsightPanel(props) {
    return (
        <div>
            <div style={phantomStyle} />
            <div id={'insight-panel'}>
                {props.children}
            </div>
        </div>
    );
}

export default InsightPanel;
