import React, { useState } from 'react';

function InfoItem(props) {
    const [expandedDescr, setExpandedDescr] = useState(false);
    return (
        <div className={'info-item'}>
            <div onClick={() => setExpandedDescr(!expandedDescr)} className={"info-item-header"}>
                {props.label}
            </div>
            {expandedDescr &&
                <div className={"info-item-description"}>
                    {props.body}
                </div>
            }
        </div>
    );
}

export default InfoItem;
