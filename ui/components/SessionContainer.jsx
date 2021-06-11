import React from "react";
import { connect } from "react-redux";
import PropTypes from 'prop-types';
import Editor from './Editor';
import ControlPanel from './ControlPanel';

import { sessionActions } from '../actions';

class SessionContainer extends React.Component {
	static propTypes = {
	};
    constructor() {
        super();
    }

	componentDidMount() {
		const {dispatch} = this.props;
    	dispatch(sessionActions.runSession());
	}

    render() {
		const {sessionState} = this.props;
		return (
			<div id='editor-area-container'>
				<ControlPanel />
				<Editor
					source={sessionState?.source_in}
					target={sessionState?.source_out}
				/>
			</div>
		)
    }
}

const mapStateToProps = (state) => {
	const {sessionState} = state.runSession;
	return {
		sessionState
	}
}

// const mapDispatchToProps = {
// 	sessionActions.runSession
// }

export default connect(mapStateToProps, null)(SessionContainer);
