import React from "react";
import { connect } from "react-redux";
import PropTypes from 'prop-types';
import CodeArea from './CodeArea';
import ControlPanel from './ControlPanel';

import { sessionActions } from '../actions';

/*

	Main Session Controller

*/

class SessionContainer extends React.Component {
	static propTypes = {
	};
    constructor() {
        super();
		this.state = {
			source_opt: 'C',
			target_opt: 'X86',
			optimization_opt: 'O0',
			program_opt: 'HelloWorld',
			target_loc: -1
		}
		this._runSession = this._runSession.bind(this);
		this._updateSessionState = this._updateSessionState.bind(this);
		this._updateSessionStatePrim = this._updateSessionStatePrim.bind(this);
    }

	componentDidUpdate() {
		// console.log("componentDidUpdate: ", this.state, this.props);
	}

	_runSession(action) {
		return function(a) {
			action(a, function() {
				const {
					source_opt,
					target_opt,
					optimization_opt,
					program_opt,
				} = this.state;
				this.props.dispatch(sessionActions.runSession(
					source_opt,
					target_opt,
					optimization_opt,
					program_opt
				));
			});
		}
	}

	_updateSessionState(stateField) {
		return ({value}, f) => {this.setState({ [stateField]: value}, f);}
	}

	_updateSessionStatePrim(stateField) {
		return (v, f) => {this.setState({[stateField]: v});}
	}

    render() {
		const {sessionState} = this.props;
		const {
			source_opt,
			target_opt,
			optimization_opt,
			program_opt,
		} = this.state;
		return (
			<div id='editor-area-container'>
				<ControlPanel
					source={source_opt}
					target={target_opt}
					optimization={optimization_opt}
					program_sample={program_opt}
					source_onChange={this._runSession(this._updateSessionState('source_opt'))}
					target_onChange={this._runSession(this._updateSessionState('target_opt'))}
					opt_onChange={this._runSession(this._updateSessionState('optimization_opt'))}
					program_sample_onChange={this._runSession(this._updateSessionState('program_opt'))}
					target_loc={this.state.target_loc}
				/>
				<CodeArea
					source={sessionState.source}
					target={sessionState.target}
					source_in={source_opt}
					target_out={target_opt}
					update_target_loc={this._updateSessionStatePrim('target_loc')}
					target_loc={this.state.target_loc}
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

export default connect(mapStateToProps, null)(SessionContainer);
