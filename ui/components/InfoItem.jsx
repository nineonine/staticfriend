import React from 'react';

function InfoItem(props) {
    return (
        <div className={'info-item'}>
            <h5>
                {props.label}
            </h5>
            <p>
                {props.body}
            </p>
        </div>
    );
}

export default InfoItem;
