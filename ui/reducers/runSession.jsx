import {
    RUN_SESSION,
    RUN_SESSION_FAILED,
    RUN_SESSION_START
} from '../actions/types';

const runSession = (state = {
    sessionState: {source: 'in', target: 'out'},
    fetching: false,
    error: undefined
}, action) => {
    switch(action.type) {
        case RUN_SESSION:
            return {
                ...state,
                sessionState: action.data,
                fetching: false,
                error: undefined,
            }
        case RUN_SESSION_FAILED:
            return Object.assign({}, state, {
                sessionState: {},
                fetching: false,
                error: action.error,
            })
        case RUN_SESSION_START:
            return {
                ...state,
                fetching: true,
                error: undefined,
            }
        default:
            return state;
    }
}

export default runSession;
